################################################################################
#               CONSTRUCT DICTIONARIES & CALCULATE SENTIMENT SCORES                          
################################################################################
# Preamble, setting working directory
rm(list = ls())
setwd("INSERT WD")

## packages
library(tibble)
library(dplyr)
library(SentimentAnalysis)
library(readr)
library(slam)
library(DescTools)

# Load data:
meta <-  readRDS("fromR/metaAdj_v4.rds") 

# Threshold:
th = 0.25
#-------------------------------------------------------------------------------
# ML Unigram, full sample 
#-------------------------------------------------------------------------------
mlUni <- read_csv("robustMNIR/ML_score_unigram.csv")

# Positive ML Unigram dictionary: 
mlUniPos <- mlUni %>% 
  filter(positive-negative >= 0.6, 
         freq < 2000,
         idf < 7
         )

# Save dictionary:
mlUniPos %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_positive_unigram.txt")
saveRDS(mlUniPos, file = "robustMNIR/mlUniPos.rds")

# Negative ML Unigram dictionary: 
mlUniNeg <- mlUni %>% 
  filter(negative-positive >= 0.59,
         freq < 2000,
         idf < 7
         ) 

# Save dictionary:
mlUniNeg %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_negative_unigram.txt")
saveRDS(mlUniNeg, file = "robustMNIR/mlUniNeg.rds")

# Calculate sentiment scores:
dtm <- readRDS("robustMNIR/dtm_unigram.rds")

mnir_senti = (row_sums(dtm[, colnames(dtm) %in% mlUniPos$word], na.rm = T) - 
                row_sums(dtm[, colnames(dtm) %in% mlUniNeg$word], na.rm = T)) / row_sums(dtm)

# Normalize sentiment scores:
mnir_senti <- Winsorize(mnir_senti, probs = c(0.01, 0.99), na.rm = T)
mnir_senti <- ((mnir_senti - min(mnir_senti, na.rm = T)) / 
                 (max(mnir_senti, na.rm = T) - 
                    min(mnir_senti, na.rm = T)))*(1 - -1) + -1

meta$mnir_sentiUni <- mnir_senti
hist(meta$mnir_sentiUni)

meta <- meta %>% 
  mutate(
    mnir_uni_neu = ifelse(mnir_sentiUni > -th & mnir_sentiUni < th, 1, 0),
    mnir_uni_pos = ifelse(mnir_sentiUni >= th, 1, 0),
    mnir_uni_neg = ifelse(mnir_sentiUni <= -th, 1, 0))


rm(mlUni, dtm, mnir_senti)
#-------------------------------------------------------------------------------
# ML bigrams, full sample 
#-------------------------------------------------------------------------------
mlBi <- read_csv("robustMNIR/ML_score_bigram.csv")

# Positive ML Bigram dictionary: 
mlBiPos <- mlBi %>% 
  filter(positive-negative >= 0.45, 
         freq <= 900
         )

# Save dictionary:
mlBiPos %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_positive_bigram.txt")
saveRDS(mlBiPos, file = "robustMNIR/mlBiPos.rds")

# Negative ML Bigram dictionary: 
mlBiNeg <- mlBi %>% 
  filter(negative-positive >= 0.45, 
         freq <= 900
         ) 

# Save dictionary:
mlBiNeg %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_negative_bigram.txt")

saveRDS(mlBiNeg, file = "robustMNIR/mlBiNeg.rds")

# Calculate sentiment scores:
dtm <- readRDS("robustMNIR/dtm_bigram.rds")

# Compute overall sentiment scores:
mnir_senti = (row_sums(dtm[, colnames(dtm) %in% mlBiPos$word], na.rm = T) - 
                row_sums(dtm[, colnames(dtm) %in% mlBiNeg$word], na.rm = T)) / row_sums(dtm)

# Normalize sentiment scores:
mnir_senti <- Winsorize(mnir_senti, probs = c(0.01, 0.99), na.rm = T)
mnir_senti <- ((mnir_senti - min(mnir_senti, na.rm = T)) / 
                 (max(mnir_senti, na.rm = T) - 
                    min(mnir_senti, na.rm = T)))*(1 - -1) + -1

meta$mnir_sentiBi <- mnir_senti
hist(meta$mnir_sentiBi)

meta <- meta %>% 
  mutate(
    mnir_bi_neu = ifelse(mnir_sentiBi > -th & mnir_sentiBi < th, 1, 0),
    mnir_bi_pos = ifelse(mnir_sentiBi >= th, 1, 0),
    mnir_bi_neg = ifelse(mnir_sentiBi <= -th, 1, 0))


#-------------------------------------------------------------------------------
# LM Unigrams, full sample
#-------------------------------------------------------------------------------
lmPos <- DictionaryLM$positive
lmNeg <- DictionaryLM$negative

dtm <- readRDS("robustMNIR/dtm_unigram.rds")

lm_senti = (row_sums(dtm[, colnames(dtm) %in% lmPos], na.rm = T) - 
                row_sums(dtm[, colnames(dtm) %in% lmNeg], na.rm = T)) / row_sums(dtm)

lm_senti <- Winsorize(lm_senti, probs = c(0.01, 0.99), na.rm = T)

hist(lm_senti)

# Normalize sentiment scores:
lm_senti <- ((lm_senti - min(lm_senti, na.rm = T)) / 
                 (max(lm_senti, na.rm = T) - 
                    min(lm_senti, na.rm = T)))*(1 - -1) + -1

max(lm_senti)
hist(lm_senti, breaks = 100)

meta$lm_senti <- lm_senti
hist(meta$lm_senti)

meta <- meta %>% 
  mutate(
    lm_neu = ifelse(lm_senti > -th & lm_senti < th, 1, 0),
    lm_pos = ifelse(lm_senti >= th, 1, 0),
    lm_neg = ifelse(lm_senti <= -th, 1, 0))

#-------------------------------------------------------------------------------
# ML & LM Unigrams, full sample
#-------------------------------------------------------------------------------
MLLMPos <- unique(c(lmPos, mlUniPos$word))
MLLMNeg <- unique(c(lmNeg, mlUniNeg$word))

ml_lm_senti = (row_sums(dtm[, colnames(dtm) %in% MLLMPos], na.rm = T) - 
              row_sums(dtm[, colnames(dtm) %in% MLLMNeg], na.rm = T)) / row_sums(dtm)

ml_lm_senti <- Winsorize(ml_lm_senti, probs = c(0.01, 0.99), na.rm = T)

hist(ml_lm_senti)

# Normalize sentiment scores:
ml_lm_senti <- ((ml_lm_senti - min(ml_lm_senti, na.rm = T)) / 
               (max(ml_lm_senti, na.rm = T) - 
                  min(ml_lm_senti, na.rm = T)))*(1 - -1) + -1

max(ml_lm_senti)
hist(ml_lm_senti, breaks = 100)

meta$ml_lm_senti <- ml_lm_senti
hist(meta$ml_lm_senti)

meta <- meta %>% 
  mutate(
    ml_lm_neu = ifelse(ml_lm_senti > -th & ml_lm_senti < th, 1, 0),
    ml_lm_pos = ifelse(ml_lm_senti >= th, 1, 0),
    ml_lm_neg = ifelse(ml_lm_senti <= -th, 1, 0))

#-------------------------------------------------------------------------------
# Save Meta file
#-------------------------------------------------------------------------------
saveRDS(meta, file = "robustMNIR/meta_ML_LM_complete.rds")

#-------------------------------------------------------------------------------
# Dictionary breadth
#-------------------------------------------------------------------------------
nrow(mlBiPos) + nrow(mlBiNeg)
nrow(mlUniPos) + nrow(mlUniNeg)



