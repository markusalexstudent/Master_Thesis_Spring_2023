################################################################################
#                                 VADER 
################################################################################
rm(list = ls())

library(vader)
library(tidyverse)
library(DescTools)

setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

# Load data:
meta_openai <- readRDS("regressions/meta_GPT_ML_LM_regression.rds")
#meta_openai <- meta_openai[1:10,]

# Run VADER on pre-processed data: 
vader <- matrix(NA, nrow = nrow(meta_openai), ncol = 5)
colnames(vader) <- c("ticker", "pos", "neu", "neg", "compound")

for(i in 1:nrow(meta_openai)) {
  
  cat("Iteration", i, "(", (i/nrow(meta_openai))*100, "%)", "\n")
  
  vader[i,1] <- meta_openai$ticker[i]
  
  senti <- get_vader(meta_openai$text[i])
  vader[i,2] <- senti[3]
  vader[i,3] <- senti[4]
  vader[i,4] <- senti[5]
  vader[i,5] <- senti[2]
  
}

vader <- data.frame(vader)
vader$compound <- as.numeric(vader$compound)

# Establish sentiment
th = 0.25
vader <- vader %>% 
  mutate(
    vader_neu = ifelse(compound > -th & compound < th, 1, 0),
    vader_pos = ifelse(compound >= th, 1, 0),
    vader_neg = ifelse(compound <= -th, 1, 0)
    )

saveRDS(vader, file = "VADER_test.rds")

meta_openai <- cbind(meta_openai, vader[,6:8])

# Regression:
reg_meta <- meta_openai %>% 
  mutate(ret_contemp = Winsorize(ret_contemp, probs = c(0.01,0.99)),
         ret_contempCapm = Winsorize(ret_contempCapm, probs = c(0.01,0.99)),
         ret_contempMkt = Winsorize(ret_contempMkt, probs = c(0.01, 0.99)),
         bm = Winsorize(bm, probs = c(0.01, 0.99)),
         turnover = Winsorize(turnover, probs = c(0.01, 0.99)), 
         mcap = Winsorize(mcap, probs = c(0.01, 0.99)))

# Add pseudo-count, as there are some volumes that are zero. (log of zero dont work)
reg_meta[which(reg_meta$turnover == 0),]$turnover <- 
  reg_meta[which(reg_meta$turnover == 0), ]$turnover + 0.00000001

reg <- list()

reg[[1]] <- lfe::felm(
  ret_contempMkt ~ log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% filter(group == "holdout"), psdef=FALSE)

reg[[2]] <- lfe::felm(
  ret_contempMkt ~ vader_pos + vader_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% filter(group == "holdout"), psdef=FALSE)

stargazer::stargazer(reg, type = "text")




