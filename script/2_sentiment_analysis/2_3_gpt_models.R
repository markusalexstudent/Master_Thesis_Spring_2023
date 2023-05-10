################################################################################
#                    CALCULATE SENTIMENT USING GPT                          
################################################################################
# Preamble, setting working directory
rm(list = ls())
options(scipen = 999)
setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

## packages
library(svMisc)                           # Progress tracker
library(readxl)                           # Load excel files                       
library(tidyverse)                        # Dplyr etc. 
library(tidytext)                         # Clean text
library(jsonlite)                         # Create json file for fine tuning
library(openai)                           # OpenAI for fine-tuning
library(DescTools)                        # Winsorize
library(TTR)
library(lubridate)                        # Manipulate dates
library(reticulate)                       # Run Python in R

# Load data:
meta <- readRDS("robustMNIR/meta_ML_LM_complete.rds")

# API Key: 
openai_key <- "[INSERT OPENAI KEY HERE]"          

#-------------------------------------------------------------------------------
# Attach pre-made sentiment analysis from GPT models (You don't need API key
# for this)
#-------------------------------------------------------------------------------
# Insert data from API: 
meta_openai <- meta[meta$group == "holdout", ]

# Order data so they have the same order as OpenAI data:
meta_openai <- meta_openai[order(meta_openai$ticker_refinitiv), ]

# Davinci:
response_list <- readRDS("openai_data/response_list_text_davinvi.rds")
meta_openai$sentiment_openai <- response_list$sentiment_openai

# Curie:
response <- readRDS("openai_data/response_curie.rds")
meta_openai$sentiment_openai_curie <- response$score

# Fine-tuned curie:
response = readRDS("openai_data/response_finetuned_curie_1000_obs.rds")
meta_openai$sentiment_curie_fine_tuned <- response$score

# Gpt-3.5-turbo: 
response <- readRDS("openai_data/response_chatgpt.rds")
meta_openai$chatgpt_senti <- tolower(response$score)

# Classification and cleaning of text:
meta_openai <- meta_openai %>% 
  mutate(
    sentiment_openai = tolower(sentiment_openai), 
    sentiment_openai = gsub("[[:punct:]]+", "", sentiment_openai),
    sentiment_openai = gsub("[ \t]", "", sentiment_openai),
    
    davinci_pos = ifelse(sentiment_openai == "positive", 1, 0),
    davinci_neg = ifelse(sentiment_openai == "negative", 1, 0),
    davinci_neu = ifelse(sentiment_openai == "neutral", 1, 0),
    
    sentiment_openai_curie = tolower(sentiment_openai_curie), 
    sentiment_openai_curie = gsub("The sentiment of the text is ", "",sentiment_openai_curie),
    sentiment_openai_curie = gsub("[[:punct:]]+", "", sentiment_openai_curie),
    sentiment_openai_curie = gsub("\n\n", "",sentiment_openai_curie),
    sentiment_openai_curie = gsub("[ \t]", "", sentiment_openai_curie),
    sentiment_openai_curie = gsub("thesentimentofthetextis", "", sentiment_openai_curie),
    sentiment_openai_curie = gsub("thesentimentofthefollowingtextis", "", sentiment_openai_curie),
    sentiment_openai_curie = gsub("thesentimentinthetextis", "", sentiment_openai_curie),
    
    curie_pos = ifelse(sentiment_openai_curie == "positive", 1, 0),
    curie_neg = ifelse(sentiment_openai_curie == "negative", 1, 0),
    curie_neu = ifelse(sentiment_openai_curie == "neutral", 1, 0),
    
    sentiment_curie_fine_tuned = tolower(sentiment_curie_fine_tuned), 
    sentiment_curie_fine_tuned = gsub("[[:punct:]]+", "", sentiment_curie_fine_tuned),
    sentiment_curie_fine_tuned = gsub("[ \t]", "", sentiment_curie_fine_tuned),
    sentiment_curie_fine_tuned = gsub("#", "", sentiment_curie_fine_tuned),
    sentiment_curie_fine_tuned = substr(sentiment_curie_fine_tuned, 1, 3),
    
    curie_pos_fine_tuned = ifelse(sentiment_curie_fine_tuned == "pos", 1, 0),
    curie_neg_fine_tuned = ifelse(sentiment_curie_fine_tuned == "neg", 1, 0),
    curie_neu_fine_tuned = ifelse(sentiment_curie_fine_tuned == "neu", 1, 0),
    
    chatgpt_senti = gsub("[[:punct:]]+", "", chatgpt_senti),
    chatgpt_senti = gsub("\n\n", "",chatgpt_senti),
    chatgpt_senti = gsub("[ \t]", "", chatgpt_senti),
    chatgpt_senti = substr(chatgpt_senti, 1, 3),
    
    chatgpt_pos = ifelse(chatgpt_senti == "pos", 1, 0),
    chatgpt_neg = ifelse(chatgpt_senti == "neg", 1, 0),
    chatgpt_neu = ifelse(chatgpt_senti == "neu" | chatgpt_senti == "mix", 1, 0)
  )

saveRDS(meta_openai, file = "regressions/meta_GPT_ML_LM_regression.rds")

#-------------------------------------------------------------------------------
# Run models (API key and positive balance on account required)
#-------------------------------------------------------------------------------
# One time operation to generate a Python environment: 
# reticulate::conda_create(envname = "open_ai", packages = "openai", python_version = "3.9")

# Once the above installation finishes, don't runt the code again.
reticulate::use_condaenv("open_ai")
openai <- import("openai")
openai$api_key <- openai_key

##------------------------------------------------------------------------------
## Run ADA, Babbage, Curie, and Davinci models
##------------------------------------------------------------------------------
# Estimated cost to run the data:
est_cost_davinci = (mean(meta_openai$words)*nrow(meta_openai)/1000)*0.02
est_cost_curier = (mean(meta_openai$words)*nrow(meta_openai)/1000)*0.002
est_cost_curier

# Below we implement an exponential backoff strategy for dealing with rate 
# limiting added by OpenAI. This is dealt with by adding increasingly long 
# delays between retries of a failed request.
# TryCatch() is used to wrap the API call. If the API call fails with an error,
# we sleep for an increasing amount of time and retry the call until it succeeds 
# or the backoff time exceeds 256 seconds (ca. 4min).
# If the API call continues to fail after that, we stop the loop and throw 
# out an error message.
# https://github.com/openai/openai-cookbook/blob/main/examples/How_to_handle_rate_limits.ipynb
response_list <- list()

start <- Sys.time()
for(i in 1:nrow(meta_openai)) {
  success <- FALSE
  backoff <- 2
  while (!success) {
    tryCatch({
      response_list[[i]] <- openai$Completion$create(
        model = "text-curie-001", # <- Insert model name here
        prompt = 
          paste("Decide wheter the sentiment of the following text is positive, neutral, or negative.\n", 
                meta_openai$text[i], "\nSentiment:", sep = ""),
        temperature = 0, # Higher temperature, higher randomness of the response
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
        #stop("Error: openai.error.RateLimitError: The server had an error while processing your request. Sorry about that!")
      }
    })
  }
  progress(i, nrow(meta_openai))
}
Sys.time() - start # Runtime 

#saveRDS(response_list, file = "openai_data/response_list_curie.rds")

#saveRDS(response_list, file = "openai_data_FINAL.rds")
#response_list <- readRDS("openai_data_FINAL.rds")

# Extract choices (i.e., output from OpenAI):
response_list <- lapply(response_list, function(x) {
  data.frame(x$choices[[1]]$text)
})

# Store results in dataframe:
response <- bind_rows(response_list)
colnames(response) <- "score"

##------------------------------------------------------------------------------
## Fine-tuning of ADA, Babbage, Curie, and Davinci models
##------------------------------------------------------------------------------
# First insert the pre-made data set that the model will be fine-tuned on:
meta_finetuned <- read.xlsx("openai_data/openai_finetuned_training_data.xlsx")
meta_finetuned <- na.omit(meta_finetuned)

# Estimated cost: 
est_cost_finetuning <- ((mean(meta_finetuned$words)*nrow(meta_finetuned))/1000)*0.03
est_cost_finetuning

# Check distribution of classification: 
nrow(meta_finetuned[meta_finetuned$classification == "positive", ])
nrow(meta_finetuned[meta_finetuned$classification == "neutral", ])
nrow(meta_finetuned[meta_finetuned$classification == "negative", ])

# Add white space: 
meta_finetuned$classification <- paste0(" ", meta_finetuned$classification)
meta_finetuned <- data.frame(prompt = paste0(meta_finetuned$text,"\n\n###\n\n"), 
                             completion = paste0(meta_finetuned$classification, "\n"))

# create a list to store the JSON objects
json_list <- list()

# iterate over the dataframe and create a JSON object for each row
for (i in 1:nrow(meta_finetuned)) {
  # create a JSON object for the row
  data <- toJSON(list(prompt = meta_finetuned[i, "prompt"], 
                      completion = meta_finetuned[i, "completion"]))
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
training_info <- upload_file(file = file, purpose = "fine-tune", 
                             openai_api_key = openai_key)

# Crate fine tuning model:
info <- create_fine_tune(
  training_file = training_info$id,
  model = c("curie"),                       # <- Select which model to fine-tune
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

###-----------------------------------------------------------------------------
### Running fine-tuned ADA, Babbage, Curie, and Davinci models
###-----------------------------------------------------------------------------
# Run latest fine-tuned model:
ft_model = ft$data[nrow(ft$data),"fine_tuned_model"]

# Create FT-sample (this is to reduce cost):
#set.seed(1)
#meta_openai_ft <- meta_openai[sample(1:1000), ]
est_cost_curie = (mean(meta_openai$words)*nrow(meta_openai)/1000)*0.012
est_cost_davinci = ((mean(meta_openai$words)*nrow(meta_openai))/1000)*0.12

response_list <- list()

start <- Sys.time()

for(i in 1:nrow(meta_openai)) {
  success <- FALSE
  backoff <- 2
  
  while (!success) {
    tryCatch({
      response_list[[i]] <- openai$Completion$create(
        model = ft_model, #"ada:ft-personal-2023-03-01-13-59-51",
        prompt = paste0(meta_openai$text[i], "\n\n###\n\n"),
        temperature = 0,  # Higher temperature, higher randomness of the response
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
        #stop("Error: openai.error.RateLimitError: The server had an error while processing your request. Sorry about that!")
      }
    })
  }
  
  progress(i, nrow(meta_openai))
  
}

# Runtime: 
Sys.time() - start 

#saveRDS(response_list, file = "openai_data/response_list_finetuned_curie_1000_obs.rds")

#response_list <- readRDS("openai_data/response_list_finetuned_ada_unpacked.rds")

# Extract choices (i.e., output from OpenAI):
response_list <- lapply(response_list, function(x) {
  data.frame(x$choices[[1]]$text)
})

# Store results in dataframe:
response <- bind_rows(response_list)
colnames(response) <- "score"

##------------------------------------------------------------------------------
## GPT-3.5 TURBO OR GPT-4 MODELS
##------------------------------------------------------------------------------
# Estimate cost:
est_cost_chatgpt3 = (mean(meta_openai$words)*nrow(meta_openai)/1000)*0.002
est_cost_chatgpt3

# Write prompt: 
chatgpt_prompt <- data.frame(role = "user",
                             content = paste("Decide wheter the sentiment of the following text is positive, neutral, or negative (use one word):", 
                                             meta_openai$raw_text, sep = " "))

chatgpt_prompt <- rbind(data.frame(role = "system", content = "Your job is to analyse the sentiment of text."),
                        chatgpt_prompt)

messages <- split(chatgpt_prompt, seq(nrow(chatgpt_prompt)))
messages <- unname(messages)
messages <- lapply(messages, as.list)

response_list <- list()

start <- Sys.time()
for(i in 1:(nrow(meta_openai)+1)) {
  success <- FALSE
  backoff <- 2
  
  while (!success) {
    tryCatch({
      response_list[[i]] <- openai$ChatCompletion$create(
        model = "gpt-3.5-turbo", 
        messages = messages[i]
      )
      success <- TRUE
    }, error = function(e) {
      Sys.sleep(backoff)
      backoff <- backoff * 2
      if (backoff > 256) {
        next
        #stop("Error: openai.error.RateLimitError: The server had an error while processing your request. Sorry about that!")
      }
    })
  }
  
  progress(i, nrow(meta_openai))
  
}
Sys.time() - start 

#saveRDS(response_list, file = "openai_data/response_list_chatgpt.rds")

response_list <- response_list[2:length(response_list)]

# Extract choices (i.e., output from OpenAI):
response_list <- lapply(response_list, function(x) {
  data.frame(x$choices[[1]]$message$content)
})

# Store results in dataframe:
response <- bind_rows(response_list)
colnames(response) <- "score"

#saveRDS(response, file = "openai_data/response_chatgpt.rds")


