################################################################################
#                   ESTIMATE ROBUST MNIR AND STRUCTURE OUTPUT       
################################################################################
rm(list = ls())

## Number of iterations for the robust MNIR algorithm
no.iterations <- 500 # (k)

## Preamble: set working directory
setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

## Load packages
library(tibble)
library(dplyr)
library(readr)
library(parallel)
library(textir)
library(tm)
library(slam)
library(DescTools)
library(Matrix)
library(quanteda)
library(readxl)
library(textmineR)
library(purrr)
library(tidyverse)
library(quanteda)
library(Matrix)

# Read Data: 
meta <- readRDS("fromR/metaAdj_v4.rds")                   # FF3, CAPM, and normal Excess returns calculated with daily data, incl. excess returns over market over 12 days 

#-------------------------------------------------------------------------------
# Functions to estimate MNIR
#-------------------------------------------------------------------------------
getMnirLoadings <- function(meta = meta,                  # Data with Y variable
                            filter = T,                   # Additional filter
                            dtm =  dtm,                   # Document term Matrix
                            idfFilter = 12, 
                            nr.clusters = 20              # number of clusters used for MNIR implementation
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

#-------------------------------------------------------------------------------
# Additional stopwords
#-------------------------------------------------------------------------------
# City names to be removed:
cities <- read_excel("cities.xlsx")
kommune <- read_excel("kommune_fylke.xlsx")
kommune$Kommune <- tolower(kommune$Kommune)
kommune$Fylke <- tolower(kommune$Fylke)
location <- tolower(c(cities$`City/town`, unique(kommune$Fylke), 
                      kommune$Kommune, "Norway", "Sweden", "Denmark", "Finland", 
                      "Canada", "United States"))

# Months to be removed:
months <- tolower(c(month.abb, month.name))

# Names to be removed:
names <- read_excel("names_ssb.xlsx") # source: 
names <- tolower(names$names)

# Remove neutral words that occour frequently:
toks.remove <- c("asa", "disclosure", "publication", "requirements", 
                 "act", "not", "distribution", "united", "states",
                 "aalborg", "aak", "aarhus", "aaa", "aa", "a",
                 "quarter", "first quarter", "second quarter", "third quarter", 
                 "fourth quarter", "aadhaar", "aacr", "aardal",
                 "australia", "canada", "norway", "ab publ", "ab", "http", 
                 "www", "acc", "vphl", "aasen", 
                 "jurisdiction","ceo", "managing", "director", 
                 "key information", "co", "ltd", "vice", "president", "aaog",
                 location, months, "isin", "link", "webcast", "english", "alia",
                 names)
saveRDS(toks.remove, file = "robustMNIR/toksRemove.rds")

rm(cities, kommune, location, names, months)
#-------------------------------------------------------------------------------
# Estimate robust MNIR (Unigrams)
#-------------------------------------------------------------------------------
toks <- tokens_ngrams(as.vector(meta$text) %>% 
                        tokens(what = "word",
                               remove_numbers =T,         # Remove numbers  
                               remove_punct = T,          # Remove punctuation
                               remove_url = T,            # Remove URLs
                               remove_symbols = T) %>%
                        tokens_remove(c(stopwords(), toks.remove)), n = 1)  # 2 = bigram, 1 = unigram

# Create corpus: 
corpus <- VCorpus(VectorSource(toks))

# create dtm: 
dtm <- DocumentTermMatrix(corpus)
saveRDS(dtm, file = "robustMNIR/dtm_unigram.rds")

dtm <- readRDS("robustMNIR/dtm_unigram.rds")
rm(corpus, toks)

# Convert the dtm to a sparse matrix (this is to be able to run it through dmr)
dtm <- sparseMatrix(i = dtm$i, 
                    j = dtm$j, 
                    x = dtm$v, 
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))

dtm <- Matrix::as.matrix(dtm)

## estimate robust MNIR
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
                                  idfFilter = 7,
                                  nr.clusters = detectCores()-1)
}

wordCount <- tibble(word = colnames(dtm), freq = col_sums(dtm), 
                    idf = TermDocFreq(dtm)$idf)

## structure and save
structureRobustMnirOutput(MNIRest = MNIRest,
                          wordCount = wordCount,
                          filePath = "robustMNIR/idf_version/ML_score_unigram.csv")

rm(MNIRest, wordCount)

#-------------------------------------------------------------------------------
# Estimate robust MNIR (Bigrams)
#-------------------------------------------------------------------------------
toks <- tokens_ngrams(as.vector(meta$text) %>% 
                        tokens(what = "word",
                               remove_numbers =T,         # Remove numbers  
                               remove_punct = T,          # Remove punctuation
                               remove_url = T,            # Remove URLs
                               remove_symbols = T) %>%
                        tokens_remove(c(stopwords(), toks.remove)), n = 2)  # 2 = bigram, 1 = unigram

# Create corpus: 
corpus <- VCorpus(VectorSource(toks))

# create dtm: 
dtm <- DocumentTermMatrix(corpus)
saveRDS(dtm, file = "robustMNIR/dtm_bigram.rds")

dtm <- readRDS("robustMNIR/dtm_bigram.rds")

dtm <- sparseMatrix(i = dtm$i,
                    j = dtm$j,
                    x = dtm$v,
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))
rm(corpus, toks)


## estimate robust MNIR
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
                                  idfFilter = 7,
                                  nr.clusters = detectCores()-1)
}

wordCount <- tibble(word = colnames(dtm), freq = col_sums(dtm), 
                    idf = TermDocFreq(dtm)$idf)

## structure and save
structureRobustMnirOutput(MNIRest = MNIRest,
                          wordCount = wordCount,
                          filePath = "robustMNIR/idf_version/ML_score_bigram.csv")

rm(MNIRest, wordCount)

# Shut down system
# system('shutdown -s')

