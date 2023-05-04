# Man vs. Machine | Master Thesis Spring 2023
## An applied study comparing a man-made lexicon, a machine learned lexicon, and OpenAI's GPT for sentiment analysis.
Master thesis in Financial Economics at NHH spring 2023. All files related to the project lies here. 
 

## Info
General Info: 
- The R-script is split into parts, and should be run chronologically. 
- Data is in the "data" folder.
  - Plots and tables are in "tables_and_plots".
  - ML lexicons are in "dictionary".
  - Robust MNIR outputs (w/ idf score) are in "robustMNIR".
  - An csv-file with all scraped press releases can be found in the data folder.
- R script is in the "script" folder.
- You do not need to run the script from "scratch", i.e. web scrape and run ML/OpenAI Models (these are very time consuming). Just download the data, set working directionary in the script and then run the script. 

What you need to run the complete script (besides packages in the script): 
- OpenAI API key. Create an account on https://openai.com/
- Refinitiv Eikon API key. (If you are a student at NHH you can ask the library for a license --> https://nhh.libguides.com/az.php)
- Docker (needed for web scraping NewsWeb) https://www.docker.com/

Below, key parts of the script is added (models, fine-tuning, packages). See the script folder for compelte script.

## Extracts from R script
### R Libraries:
```r 
library(eikonapir)                        # Refinitiv Eikon API for R
library(RSelenium)                        # Docker and Selenium
library(rvest)                            # Webscraping
library(svMisc)                           # Progress tracker
library(tidyverse)                        # Dplyr etc. 
library(tidytext)                         # Clean text
library(textmineR)                        # DTM and TF-IDF filtering
library(tm)                               # Textmining (incl. stopwords)
library(textcat)                          # Detect language
library(cld2)                             # Detect language (superior to textcat --> https://stackoverflow.com/questions/8078604/detect-text-language-in-r)
library(parallel)                         # Parallel computing
library(readxl)                           # Load excel files
library(lubridate)                        # Manipulate dates
library(PerformanceAnalytics)             # Calculate returns etc. 
library(quantmod)                         # Stopwords, textual analysis etc.
library(quanteda)                         # Text analysis
library(data.table)                       # Work with tables
library(textir)                           # MNIR ML Model
library(rrapply)                          # recursively apply a function to elements of a nested list based on a general condition function
library(kableExtra)                       # Create latex tables
library(multidplyr)                       # Dplyr with parallel 
library(openxlsx)                         # Save and read .xlsx
library(bizdays)                          # Business days 
library(reticulate)                       # Run Python in R
library(remotes)                          # Install remote repos (e.g., github)
library(stringr)                          # Use wrappers
library(slam)                             # Sparse Lightweight Arrays and Matrices --> convert DTM to sparse matrix
library(SentimentAnalysis)                # LM dictionary
library(felm)                             # Panel data regressions
library(wordcloud)                        # Wordcloud
library(wordcloud2)                       # Wordcloud2
library(jsonlite)                         # Create json file for fine tuning
library(openai)                           # OpenAI for fine-tuning
library(DescTools)                        # Winsorize
library(wordcloud2)                       # devtools::install_github("lchiffon/wordcloud2")

```
### Refinitiv Eikon API for R:
```r
install_github("ahmedmohamedali/eikonapir") # Installation of Eikon API for R

# Log on Refinitiv Desktop
# Get API key --> APPKEY --> Register new app (API: Eikon Data API)
# Find data keys --> Data Item Browser

```

### One time operation to generate a Python environment: 
```r
reticulate::conda_create(envname = "open_ai", packages = "openai", python_version = "3.9")
# Once the above installation finishes, don't runt the code again.

reticulate::use_condaenv("open_ai")
openai <- import("openai")

# Add API key
openai$api_key <- openai_key

```
### Web scraping NewsWeb:
```r

#-------------------------------------------------------------------------------
# 1.1 Start Docker for web scraping
#-------------------------------------------------------------------------------
# Open Docker:
system("start docker")

# Start new container on Docker:
system('docker run -d -p 4445:4444 selenium/standalone-firefox')

remDr <-  RSelenium::remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

#-------------------------------------------------------------------------------
# 1.2 Scrape URL + meta data
#-------------------------------------------------------------------------------
#Start the remote driver
remDr$open()

# List elemts to iterate with parallel processing: 
category <- "INSERT CATEGORY TO SCRAPE HERE, E.G. '1005'"
urls_list <- c()
dates <- seq(as.Date("2013-01-01"), as.Date("2023-01-31"), by = "1 day")

for(i in 1:length(dates[-1])) {
  urls_list[i] <- paste0("https://newsweb.oslobors.no/search?category=",category,
                         "&issuer=&fromDate=",dates[i],
                         "&toDate=", dates[i],
                         "&market=&messageTitle=") #dates[-length(dates)][i]
  print(urls_list[i])
}

# Create list to store dataframes in:
df_list <- list()
message_url_list <- list()

start = Sys.time()

# Extract meta data for all links in url list:
for(i in seq_along(urls_list)) {
  
  remDr$navigate(urls_list[[i]])
  
  Sys.sleep(1) # This is added so that there is time to load each page in FireFox
  
  doc <- read_html(remDr$getPageSource()[[1]])
  
  # Extract df: 
  df_list[[i]] <- doc %>%
    html_table() %>% 
    data.frame()
  
  # Extract links to text:
  message_url_list[[i]] <- doc %>% 
    html_nodes("a") %>% html_attr('href') %>% 
    data.frame() 
  
  #print(message_url_list[[i]])
  
  progress(i, length(urls_list)) # Track progress of loop    
}

# Remove the NULL elements from the list
df_list <- Filter(function(x) nrow(x) > 0, df_list)

# Merge dataframes within each list:
df <- bind_rows(df_list)
message_urls <- bind_rows(message_url_list)

# Clean data:
message_urls <- data.frame(message_urls[grep("message", message_urls$.),])
colnames(message_urls) <- "message"

# Merge dataframes:
df$index = seq(1:nrow(df))
message_urls$index = seq(1:nrow(df))
df <- merge(df, message_urls, by = "index")

# Create an url column:
df <- df %>% 
  mutate(url_complete = paste0("https://newsweb.oslobors.no", message))

# Save df: 
# saveRDS(df, file = "df_inside_information.rds")
# df <- readRDS("df.rds")


#-------------------------------------------------------------------------------
# 1.3 Text scraping
#-------------------------------------------------------------------------------

# Create list to store text in:
message_texts_list <- list()

# Loop through each url: 
for (i in seq_along(df$url_complete)) {
  
  remDr$navigate(df$url_complete[i])
  
  Sys.sleep(1) # This is added so that there is time to load each page in FireFox
  
  docs <- read_html(remDr$getPageSource()[[1]])
  
  message_texts <- docs %>% 
    html_nodes(xpath = '/html/body/div/div/main/div[2]/div[2]/div[2]/div/text()') %>% 
    html_text()
  
  message_texts_list[[i]] <- message_texts
  
  #print(message_texts) # Print text to check if errors/missing values occour
  
  progress(i, length(df$url_complete)) # Track progress of loop
  
}

# saveRDS(message_texts_list, file = "message_texts_list_inside_information.rds")


# Close docker an Selenium:
remDr$close()
gc()
system("taskkill /im docker.exe /f", intern=FALSE, ignore.stdout=FALSE)
#system('shutdown -s')

Sys.time() - start



```

### Example of API call to OpenAI:
```r
response_list <- list()

start <- Sys.time()

for(i in 1:nrow(meta_openai)) {
  success <- FALSE
  backoff <- 2
  
  while (!success) {
    tryCatch({
      response_list[[i]] <- openai$Completion$create(
        model = "text-curie-001",                                    # Model name.
        prompt = paste("Decide wheter the sentiment of the following text is positive, neutral, or negative.\n", 
         meta_openai$text[i], 
         "\nSentiment:", sep = ""),
        temperature = 0,                                             # Higher temperature, higher randomness of the response
        max_tokens = 32L,
        top_p = 1,
        frequency_penalty = 0, 
        presence_penalty = 0
      )
      success <- TRUE
    }, error = function(e) {
      Sys.sleep(backoff)
      backoff <- backoff * 2
      if (backoff > 256) {
        next
        #stop("error")
      }
    })
  }
  
  progress(i, nrow(meta_openai))
  
}

# Runtime: 
Sys.time() - start 

# Extract choices (i.e., output from OpenAI):
response_list <- lapply(response_list, function(x) {
  data.frame(x$choices[[1]]$text)
})
  
# Store results in dataframe:
response <- bind_rows(response_list)
colnames(response) <- "score"
meta_openai$sentiment_openai <- response$score
```

### Example of fine-tuned OpenAI model: 
```r
#-------------------------------------------------------------------------------
# Create FT-model
#-------------------------------------------------------------------------------
meta_finetuned <- read.xlsx("openai_data/openai_finetuned_training_data.xlsx")
meta_finetuned <- na.omit(meta_finetuned)

# Add white space: 
meta_finetuned$classification <- paste0(" ", meta_finetuned$classification)
meta_finetuned <- data.frame(prompt = paste0(meta_finetuned$text,"\n\n###\n\n"), 
                             completion = paste0(meta_finetuned$classification, "\n"))

# create a list to store the JSON objects
json_list <- list()

# iterate over the dataframe and create a JSON object for each row
for (i in 1:nrow(meta_finetuned)) {
  # create a JSON object for the row
  data <- toJSON(list(prompt = meta_finetuned[i, "prompt"], completion = meta_finetuned[i, "completion"]))
  # append the JSON object to the list
  json_list[[i]] <- data
}

# combine the list of JSON objects into a JSONL string
jsonl_string <- paste(json_list, collapse = "\n")
jsonl_string <- gsub("\\[|\\]", "", jsonl_string) # String must be cleaned for "[" "]" to be able to upload

# write the JSONL string to a file
cat(jsonl_string, file = "prompt_curie_finetuned.jsonl")

# Create path to upload file
file = paste0(getwd(), "/prompt_curie_finetuned.jsonl")
training_info <- upload_file(file = file, purpose = "fine-tune", openai_api_key = openai_key)

# Crate fine tuning model:
info <- create_fine_tune(
  training_file = training_info$id,
  model = c("curie"),
  n_epochs = 4,
  batch_size = NULL,
  learning_rate_multiplier = NULL,
  prompt_loss_weight = 0.1,
  compute_classification_metrics = FALSE,
  classification_n_classes = NULL,
  classification_positive_class = NULL,
  classification_betas = NULL,
  suffix = NULL,
  openai_api_key = openai_key,
  openai_organization = NULL
)

# View status of all models:
ft <- list_fine_tunes(
  openai_api_key = openai_key,
  openai_organization = NULL
)

# Check status on fin-tuned model:
ft$data[nrow(ft$data),"status"]

# Select (LATEST) model: 
fine_tuned_model_nr <- nrow(ft$data)

# Check status of fine tuned model: 
retrieve_fine_tune(
  ft$data$id[fine_tuned_model_nr],
  openai_api_key = openai_key,
  openai_organization = NULL
)

# List all models: 
all_models <- list_models(
  openai_api_key = openai_key
)

all_models <- data.frame(all_models$data)

#-------------------------------------------------------------------------------
# Run FT-model on test data
#-------------------------------------------------------------------------------
# Run latest fine-tuned model:
ft_model = ft$data[nrow(ft$data),"fine_tuned_model"]

response_list <- list()

start <- Sys.time()

for(i in 1:nrow(meta_openai)) {
  success <- FALSE
  backoff <- 2
  
  while (!success) {
    tryCatch({
      response_list[[i]] <- openai$Completion$create(
        model = ft_model,                                                     
        prompt = paste0(meta_openai$text[i], "\n\n###\n\n"),
        temperature = 0,                                                      
        max_tokens = 32L,
        top_p = 1,
        frequency_penalty = 0, 
        presence_penalty = 0
      )
      success <- TRUE
    }, error = function(e) {
      Sys.sleep(backoff)
      backoff <- backoff * 2
      if (backoff > 256) {
        next
        #stop("error")
      }
    })
  }
  progress(i, nrow(meta_openai))
}

# Runtime: 
Sys.time() - start 

# Extract choices (i.e., output from OpenAI):
response_list <- lapply(response_list, function(x) {
  data.frame(x$choices[[1]]$text)
})

# Store results in dataframe:
response <- bind_rows(response_list)
colnames(response) <- "score"


```


### Example of robust MNIR function (tweaked with IDF):
Inspired by: 
- Taddy: https://arxiv.org/abs/1012.2098
- Garcia: https://www.sciencedirect.com/science/article/abs/pii/S0304405X22002422

```r
#-------------------------------------------------------------------------------
# Functions to estimate MNIR
#-------------------------------------------------------------------------------
getMnirLoadings <- function(meta = meta,                  # Data with Y variable
                            filter = T,                   # Additional filter
                            dtm =  dtm,                   # Document term Matrix
                            idfFilter = 12,               # Max IDF value
                            nr.clusters = detectCores()-1 # number of clusters used for MNIR implementation
){
  
  ## This function just needs a meta object
  ## with a filing.period.excess.return variable.
  ## The PERMNO+ information provided in the repository 
  ## should make this easily available to many researchers.
  ## But we note this function will not run without an updated
  ## metadata file (relative to what we share in the repository).
  
  ## initiate cluster
  cl <- makeCluster(nr.clusters)
  
  ## Filter 1: limit with provided filter
  meta <- meta[filter, ]
  dtm <- dtm[filter, ]
  
  ## Filter 2: take out empty documents
  filter2 <- row_sums(dtm) != 0
  meta <- meta[filter2, ]
  dtm <- dtm[filter2, ]
  
  ## Filter 3: take out empty terms
  filter3 <- col_sums(dtm) != 0
  dtm <- dtm[, filter3]
  
  ## Filter 4: Limit by idf value
  filter4 <- TermDocFreq(dtm) %>% 
    filter(idf <= idfFilter) %>% 
    select(term) %>% 
    as_vector()
  dtm <- dtm[,filter4]
  
  # Winsorize at 1-99%
  meta$ret_contempMkt <- Winsorize(meta$ret_contempMkt, probs = c(0.01, 0.99))
  
  ## Fit the MNIR model
  fits <- dmr(cl,
              covars = meta$ret_contempMkt, 
              counts = dtm, 
              bins = NULL,
              gamma = 0, 
              nlambda = 10,
              verb = 2)
  
  ## Extract MNIR coefs
  mnir.coef <- sort(coef(fits)[2,])
  
  ## end cluster
  stopCluster(cl)
  
  ## output
  return(mnir.coef)
}

structureRobustMnirOutput <- function(MNIRest = MNIRest,  # output of function getMnirLoadings
                                      wordCount,          # word count 
                                      filePath = NULL     # output destination
){ 
  
  # Adjust colname
  names(MNIRest) <- 1:length(MNIRest)
  
  # make into a tibble
  lapply(names(MNIRest), function(cn){
    temp <- tibble(word = names(MNIRest[[cn]]))
    temp[[cn]] <- 0
    temp[[cn]][0 > as.vector(MNIRest)[[cn]]] <- -1
    temp[[cn]][0 < as.vector(MNIRest)[[cn]]] <- 1
    temp
  }) -> MNIRest
  
  # aggregate
  out <- MNIRest[[1]]
  for(i in 2:length(MNIRest)){out <- full_join(out, MNIRest[[i]])}
  MNIRest <- out
  rm(out) 
  
  # summarise 
  MNIRest[,-1] %>% 
    apply(., 1, function(x){
      c(positive = sum(x == 1, na.rm=T), 
        negative = sum(x == -1, na.rm=T), 
        missing = sum(is.na(x)))
    }) %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(word = MNIRest[,1]) -> MNIRest
  MNIRest$word <- unlist(MNIRest$word)
  names(MNIRest$word) <- NULL
  
  # Add information 
  MNIRest %>%
    left_join(wordCount) %>%
    arrange(-(positive-negative)) %>%
    mutate(positive = positive / no.iterations) %>%
    mutate(negative = negative / no.iterations) %>%
    select(word, positive, negative, freq, idf) -> MNIRest
  
  # save
  write.csv(MNIRest, file = filePath, row.names = F)
  
  # end
  return(NULL)}
```
### Running robust MNIR w/ IDF
```r
MNIRest <- list()

for(i in 1:no.iterations){
  cat("Iteration", i, "\n")
  
  # randomly select 5000 observations for each iteration
  filter.sample <- (1:nrow(meta[meta$group == "training", ])) %in% 
    (sample(1:nrow(meta[meta$group == "training", ]), 5000)) # (q)
  
  # Call getMnirLoadings function
  MNIRest[[i]] <- getMnirLoadings(meta = meta[meta$group == "training", ],
                                  filter = filter.sample,
                                  dtm =  dtm[meta$group == "training", ],
                                  idfFilter = 7, #12
                                  nr.clusters = detectCores()-1)
}

wordCount <- tibble(word = colnames(dtm), freq = col_sums(dtm), 
                    idf = TermDocFreq(dtm)$idf)

## structure and save
structureRobustMnirOutput(MNIRest = MNIRest,
                          wordCount = wordCount,
                          filePath = "robustMNIR/ML_score_unigram.csv")
```

