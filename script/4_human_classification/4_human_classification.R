################################################################################
#                             Human classification                         
################################################################################
rm(list = ls())
options(scipen = 999)

library(tidyverse)                        # Dplyr etc. 
library(tidytext)                         # Clean text
library(readxl)                           # Load excel files
library(lubridate)                        # Manipulate dates

setwd("[INSERT WORKING DIRECTORY]")

meta_openai <- readRDS("regressions/meta_GPT_ML_LM_regression.rds")
#-------------------------------------------------------------------------------
# Create random sample for human sentiment testing
#-------------------------------------------------------------------------------
set.seed(1)
metaHumanTesting <- meta_openai[sample(nrow(meta_openai), 500),]
metaHumanTesting <- metaHumanTesting %>% 
  select(ticker_refinitiv, ticker, date, time, market, title, raw_text, url, words, lang, ISIN, trbc_industry)
#openxlsx::write.xlsx(metaHumanTesting, file = "fromR/Manual_Human_Sentiment_Test/Manual_Human_Sentiment_test.xlsx")

#-------------------------------------------------------------------------------
# Load human sentiment classification data
#-------------------------------------------------------------------------------
metaHumanTesting <- read_xlsx("fromR/Manual_Human_Sentiment_Test/Manual_Human_Sentiment_test_finished.xlsx")

colnames(metaHumanTesting) <- c("ticker_refinitiv", "ticker", "date", "time", "market", "title", "raw_text", "url",
                                "words", "lang", "ISIN", "trbc_industry", "sentiFinLitHuman", "sentiFinNonLitHuman")
metaHumanTesting <- metaHumanTesting %>% 
  mutate(FinLitHu_pos = ifelse(sentiFinLitHuman == "positive", 1, 0),
         FinLitHu_neg = ifelse(sentiFinLitHuman == "negative", 1, 0),
         FinLitHu_neu = ifelse(sentiFinLitHuman == "neutral", 1, 0),
         NonFinLitHu_pos = ifelse(sentiFinNonLitHuman == "positive", 1, 0),
         NonFinLitHu_neg = ifelse(sentiFinNonLitHuman == "negative", 1, 0),
         NonFinLitHu_neu = ifelse(sentiFinNonLitHuman == "neutral", 1, 0)
  ) %>% 
  select(url, FinLitHu_pos, FinLitHu_neg, FinLitHu_neu,
         NonFinLitHu_pos, NonFinLitHu_neg, NonFinLitHu_neu)

metaHumanTesting <- merge(metaHumanTesting, meta_openai, by = "url")

saveRDS(metaHumanTesting, file = "fromR/Manual_Human_Sentiment_Test/metaHumanTesting.rds")
