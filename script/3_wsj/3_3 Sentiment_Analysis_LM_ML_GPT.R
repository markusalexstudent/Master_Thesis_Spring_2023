################################################################################
#                           Testing external validity
################################################################################
rm(list = ls())
options(scipen = 999)
Sys.setlocale("LC_CTYPE", "nb_NO.utf8")
library(tidyverse)                        # Dplyr etc. 
library(tidytext)                         # Clean text
library(readxl)                           # Load excel files
library(lubridate)                        # Manipulate dates
library(quanteda)                         # Tokenizer
library(tm)
library(DescTools)

setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

meta_wsj <- readRDS("external_validity_wsj/meta_wsjAdj.rds")
th <- 0.25

# Add word count and filter meta_wsj: 
meta_wsj <- meta_wsj %>% 
  mutate(words = str_count(raw_text,  pattern = "\\w+"),
         numbers = str_count(raw_text, pattern = "\\d+"),
         wn = words + numbers,
         month = month(date),
         weekday = weekdays(date),
         yearMonth = paste0(year(date), month(date))) %>% 
  filter(wn >= 50, 
         wn <= 2000)

#-------------------------------------------------------------------------------
# Test ML and LM on unigrams
#-------------------------------------------------------------------------------
# Tokenize:
toks <- tokens_ngrams(
  as.vector(meta_wsj$text) %>% 
    tokens(what = "word",
           remove_numbers =T,                                           
           remove_punct = T,                                              
           remove_url = T,                                             
           remove_symbols = T) %>% 
    tokens_remove(stopwords()),
  n = 1)                                                           

# Create corpus: 
corpus <- VCorpus(VectorSource(toks))

# create dtm: 
dtm <- DocumentTermMatrix(corpus)

# import dictionaries:
mnir_top_pos <- read.delim("dictionary/ML_positive_unigram.txt", sep = ",")
mnir_top_pos[nrow(mnir_top_pos)+1, 1] <- colnames(mnir_top_pos)
colnames(mnir_top_pos) <- "ngram"

mnir_top_neg <- read.delim("dictionary/ML_negative_unigram.txt", sep = ",")
mnir_top_neg[nrow(mnir_top_neg)+1, 1] <- colnames(mnir_top_neg)
colnames(mnir_top_neg) <- "ngram"

# Compute overall sentiment scores:
mnir_senti = (row_sums(dtm[, colnames(dtm) %in% mnir_top_pos$ngram], na.rm = T) -
                row_sums(dtm[, colnames(dtm) %in% mnir_top_neg$ngram], na.rm = T)) / row_sums(dtm)

# Normalize the data:
mnir_senti <- Winsorize(mnir_senti, probs = c(0.01, 0.99), na.rm = T)
mnir_senti <- ((mnir_senti - min(mnir_senti, na.rm = T)) / 
                 (max(mnir_senti, na.rm = T) - min(mnir_senti, na.rm = T)))*(1 - -1) + -1

meta_wsj$mnir_sentiUni<- mnir_senti
hist(meta_wsj$mnir_sentiUni)

meta_wsj <- meta_wsj %>% 
  mutate(
    mnir_uni_neu = ifelse(mnir_senti > -th & mnir_senti < th, 1, 0),
    mnir_uni_pos = ifelse(mnir_senti >= th, 1, 0),
    mnir_uni_neg = ifelse(mnir_senti <= -th, 1, 0)
  )

# Compute overall sentiment scores: 
lm_senti = (row_sums(dtm[, colnames(dtm) %in% DictionaryLM$positive], na.rm = T) - 
              row_sums(dtm[, colnames(dtm) %in% DictionaryLM$negative], na.rm = T)) / row_sums(dtm)

# Normalize the data:
lm_senti <- Winsorize(lm_senti, probs = c(0.01, 0.99), na.rm = T)
lm_senti <- ((lm_senti - min(lm_senti, na.rm = T)) / 
               (max(lm_senti, na.rm = T) - min(lm_senti, na.rm = T)))*(1 - -1) + -1

meta_wsj$lm_senti = lm_senti

meta_wsj <-meta_wsj %>% 
  mutate(
    lm_neu = ifelse(lm_senti > -th & lm_senti < th, 1, 0),
    lm_pos = ifelse(lm_senti >= th, 1, 0),
    lm_neg = ifelse(lm_senti <= -th, 1, 0)
  )
hist(meta_wsj$lm_senti)

#-------------------------------------------------------------------------------
# Test ML on bigrams
#-------------------------------------------------------------------------------
# Tokenize:
toks <- tokens_ngrams(
  as.vector(meta_wsj$text) %>% 
    tokens(what = "word",
           remove_numbers =T,                                             # Remove numbers  
           remove_punct = T,                                              # Remove punctuation
           remove_url = T,                                                # Remove URLs
           remove_symbols = T) %>% 
    tokens_remove(stopwords()),
  n = 2)                                                                  # 2 = bigram, 1 = unigram

# Create corpus: 
corpus <- VCorpus(VectorSource(toks))

# create dtm: 
dtm <- DocumentTermMatrix(corpus)

# import dictionaries:
mnir_top_pos <- read.delim("dictionary/ML_positive_bigram.txt", sep = ",")
mnir_top_pos[nrow(mnir_top_pos)+1, 1] <- colnames(mnir_top_pos)
colnames(mnir_top_pos) <- "ngram"

mnir_top_neg <- read.delim("dictionary/ML_negative_bigram.txt", sep = ",")
mnir_top_neg[nrow(mnir_top_neg)+1, 1] <- colnames(mnir_top_neg)
colnames(mnir_top_neg) <- "ngram"

# Compute overall sentiment scores:
mnir_senti = (row_sums(dtm[, colnames(dtm) %in% mnir_top_pos$ngram], na.rm = T) - 
                row_sums(dtm[, colnames(dtm) %in% mnir_top_neg$ngram], na.rm = T)) / row_sums(dtm)

# Normalize the data:
mnir_senti <- Winsorize(mnir_senti, probs = c(0.01, 0.99), na.rm = T)
mnir_senti <- ((mnir_senti - min(mnir_senti, na.rm = T)) / 
                 (max(mnir_senti, na.rm = T) - min(mnir_senti, na.rm = T)))*(1 - -1) + -1

meta_wsj$mnir_sentiBi<- mnir_senti
hist(meta_wsj$mnir_sentiBi)

meta_wsj <- meta_wsj %>% 
  mutate(
    mnir_bi_neu = ifelse(mnir_senti > -th & mnir_senti < th, 1, 0),
    mnir_bi_pos = ifelse(mnir_senti >= th, 1, 0),
    mnir_bi_neg = ifelse(mnir_senti <= -th, 1, 0)
  )

#-------------------------------------------------------------------------------
# GPT-3.5-TURBO
#-------------------------------------------------------------------------------
response <- readRDS("openai_data/wsj_response_chatgpt.rds")
meta_wsj$chatgpt_senti <- tolower(response$score)

meta_wsj <- meta_wsj %>% 
  mutate(chatgpt_senti = gsub("[[:punct:]]+", "", chatgpt_senti),
         chatgpt_senti = gsub("\n\n", "",chatgpt_senti),
         chatgpt_senti = gsub("[ \t]", "", chatgpt_senti),
         chatgpt_senti = substr(chatgpt_senti, 1, 3),
         
         
         chatgpt_pos = ifelse(chatgpt_senti == "pos", 1, 0),
         chatgpt_neg = ifelse(chatgpt_senti == "neg", 1, 0),
         chatgpt_neu = ifelse(chatgpt_senti == "neu" | chatgpt_senti == "mix", 1, 0)
  ) %>% 
  filter(chatgpt_senti %in% c("pos", "neg", "neu"))


saveRDS(meta_wsj, file = "external_validity_wsj/meta_wsjRegression_v2")

#-------------------------------------------------------------------------------
# WSJ Regressions | Run
#-------------------------------------------------------------------------------
reg_meta <- meta_wsj %>% 
  mutate(ret_contemp = Winsorize(ret_contemp, probs = c(0.01,0.99)),
         bm = Winsorize(bm, probs = c(0.01, 0.99)),
         turnover = Winsorize(turnover, probs = c(0.01, 0.99)), 
         mcap = Winsorize(mcap, probs = c(0.01, 0.99))  
  )

reg_meta[which(reg_meta$turnover == 0),]$turnover <- 
  reg_meta[which(reg_meta$turnover == 0), ]$turnover + 0.00000001

reg <- list()

reg[[1]] <- lfe::felm(
  ret_contemp ~ log(words) + log(bm) + 
    log(turnover) + log(mcap)
  | industry + yearMonth + weekday | 0 | industry + yearMonth + weekday,
  data = reg_meta)

reg[[2]] <- lfe::felm(
  ret_contemp ~ lm_pos + lm_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | industry + yearMonth + weekday | 0 | industry + yearMonth + weekday,
  data = reg_meta, psdef=FALSE)

reg[[3]] <- lfe::felm(
  ret_contemp ~ mnir_uni_pos + mnir_uni_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | industry + yearMonth + weekday | 0 | industry + yearMonth + weekday,
  data = reg_meta, psdef=FALSE)

reg[[4]] <- lfe::felm(
  ret_contemp ~ mnir_bi_pos + mnir_bi_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | industry + yearMonth + weekday | 0 | industry + yearMonth + weekday,
  data = reg_meta, psdef=FALSE)

reg[[5]] <- lfe::felm(
  ret_contemp ~ chatgpt_pos + chatgpt_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)   
  | industry + yearMonth + weekday | 0 | industry + yearMonth + weekday,
  data = reg_meta, psdef=FALSE)

reg[[6]] <- lfe::felm(
  ret_contemp ~ lm_pos + lm_neg + 
    + mnir_uni_pos + mnir_uni_neg +
    mnir_bi_pos + mnir_bi_neg +
    chatgpt_pos + chatgpt_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)   
  | industry + yearMonth + weekday | 0 | industry + yearMonth + weekday,
  data = reg_meta, psdef=FALSE)

# Output for R:
stargazer::stargazer(reg, 
                     type = "text", 
                     keep.stat = c("n", "adj.rsq"))


#-------------------------------------------------------------------------------
# Analyze WSJ data using OpenAI gpt-3.5-turbo (do not run)
#-------------------------------------------------------------------------------
est_cost_chatgpt3 = (mean(meta_wsj$words)*nrow(meta_wsj)/1000)*0.002
est_cost_chatgpt3

# Write prompt: 
chatgpt_prompt <- data.frame(role = "user",
                             content = paste("Decide wheter the sentiment of the following text is positive, neutral, or negative (use one word):", 
                                             meta_wsj$raw_text, sep = " "))

chatgpt_prompt <- rbind(data.frame(role = "system", content = "Your job is to analyse the sentiment of text."), chatgpt_prompt)

messages <- split(chatgpt_prompt, seq(nrow(chatgpt_prompt)))
messages <- unname(messages)
messages <- lapply(messages, as.list)

response_list <- list()

start <- Sys.time()
for(i in 1:(nrow(meta_wsj)+1)) {
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
  
  progress(i, nrow(meta_wsj))
  
}
Sys.time() - start 

#saveRDS(response_list, file = "openai_data/wsj_response_list_chatgpt.rds")

response_list <- response_list[2:length(response_list)]

# Extract choices (i.e., output from OpenAI):
response_list <- lapply(response_list, function(x) {
  data.frame(x$choices[[1]]$message$content)
})

# Store results in dataframe:
response <- bind_rows(response_list)
colnames(response) <- "score"

#saveRDS(response, file = "openai_data/wsj_response_chatgpt.rds")


