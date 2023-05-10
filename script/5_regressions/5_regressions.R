################################################################################
#                                 Regressions                         
################################################################################
# Preamble, setting working directory
rm(list = ls())
setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/Data")

## packages
library(stargazer)
library(lfe)
library(tidyverse)
library(DescTools)

# Load data:
meta_openai <- readRDS("regressions/meta_GPT_ML_LM_regression.rds")

################################################################################
#                                   NewsWeb
################################################################################
#-------------------------------------------------------------------------------
# NewsWeb Regressions | Adjustment
#-------------------------------------------------------------------------------
# Winsorize:
reg_meta <- meta_openai %>% 
  mutate(ret_contemp = Winsorize(ret_contemp, probs = c(0.01,0.99)),
         ret_contempCapm = Winsorize(ret_contempCapm, probs = c(0.01,0.99)),
         ret_contempMkt = Winsorize(ret_contempMkt, probs = c(0.01, 0.99)),
         bm = Winsorize(bm, probs = c(0.01, 0.99)),
         turnover = Winsorize(turnover, probs = c(0.01, 0.99)), 
         mcap = Winsorize(mcap, probs = c(0.01, 0.99)))

# Add pseudo-count, as there are some volumes that are zero. (log of zero don't work)
reg_meta[which(reg_meta$turnover == 0),]$turnover <- 
  reg_meta[which(reg_meta$turnover == 0), ]$turnover + 0.00000001

#-------------------------------------------------------------------------------
# NewsWeb Regressions | Run
#-------------------------------------------------------------------------------
reg = list()

reg[[1]] <- lfe::felm(
  ret_contempMkt ~ log(words) + log(bm) + 
    log(turnover) + log(mcap)
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[2]] <- lfe::felm(
  ret_contempMkt ~ lm_pos + lm_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[3]] <- lfe::felm(
  ret_contempMkt ~ mnir_uni_pos + mnir_uni_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[4]] <- lfe::felm(
  ret_contempMkt ~ mnir_bi_pos + mnir_bi_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[5]] <- lfe::felm(
  ret_contempMkt ~ chatgpt_pos + chatgpt_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[6]] <- lfe::felm(
  ret_contempMkt ~ lm_pos + lm_neg + 
    mnir_uni_pos + mnir_uni_neg + mnir_bi_pos + mnir_bi_neg +
    chatgpt_pos + chatgpt_neg +
    log(words) + log(bm) + log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[7]] <- lfe::felm(
  ret_contempMkt ~ davinci_pos + davinci_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[8]] <- lfe::felm(
  ret_contempMkt ~ curie_pos + curie_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[9]] <- lfe::felm(
  ret_contempMkt ~ curie_pos_fine_tuned + curie_neg_fine_tuned + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[10]] <- lfe::felm(
  ret_contempMkt ~ chatgpt_pos + chatgpt_neg + 
    davinci_pos + davinci_neg + 
    curie_pos + curie_neg + 
    curie_pos_fine_tuned + curie_neg_fine_tuned + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

# Fix code so that "t" is removed from the t-values
get_ts <- function(fm) {summary(fm)$coefficients[,3]}
get_pvals <- function(fm) {summary(fm)$coefficients[,4]}
ts <- lapply(reg, get_ts)
pvals <- lapply(reg, get_pvals)

# Output for R:
stargazer::stargazer(reg, 
                     type = "text",
                     digits = 3, 
                     keep.stat = c("n", "rsq","adj.rsq"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)

#-------------------------------------------------------------------------------
# Create output tables
#-------------------------------------------------------------------------------
# LM vs. ML vs. GPT
# Main regression table output for paper (with control variables):
stargazer::stargazer(reg[c(1:6)], 
                     type = "latex", 
                     out = "Regressions/output/table1.tex",
                     column.sep.width = "0.75",
                     font.size = "small",
                     keep.stat = c("n","adj.rsq"),
                     dep.var.labels = "Buy-and-Hold Abnormal Return",
                     digits = 3, 
                     covariate.labels = c("LM Positive", "LM Negative",
                                          "ML (uni) Positive", "ML (uni) Negative",
                                          "ML (bi) Positive", "ML (bi) Negative",
                                          "GPT-3.5-Turbo Positive", "GPT-3.5-Turbo Negative",
                                          "log(Words)", "log(BM)", "log(Turnover)",
                                          "log(Market Cap.)", "Euronext Growth"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)

# Main regression table output for paper (without control variables):
stargazer::stargazer(reg[c(1:6)], 
                     type = "latex", 
                     out = "Regressions/output/table2.tex",
                     column.sep.width = "0.5",
                     font.size = "small",
                     omit = c("words", "bm", "turnover", "mcap", "(1)"),
                     keep.stat = c("n","adj.rsq"),
                     dep.var.labels = "Buy-and-Hold Abnormal Return",
                     digits = 3, 
                     covariate.labels = c("LM Positive", "LM Negative",
                                          "ML (uni) Positive", "ML (uni) Negative",
                                          "ML (bi) Positive", "ML (bi) Negative",
                                          "GPT-3.5-Turbo Positive", "GPT-3.5-Turbo Negative"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)

# OpenAI Models: 
openaiReg <- reg[c(1,5,7,8,9,10)]
ts <- lapply(openaiReg, get_ts)
pvals <- lapply(openaiReg, get_pvals)

# Horse race regression table between GPT-3 models (output):
stargazer::stargazer(openaiReg, 
                     type = "latex", 
                     out =  "Regressions/output/table3.tex",
                     column.sep.width = "0.75",
                     font.size = "small",
                     keep.stat = c("n", "adj.rsq"),
                     omit = c("words", "bm", "turnover", "mcap", "(1)"),
                     dep.var.labels = "Buy-and-Hold Abnormal Return",
                     digits = 3, 
                     covariate.labels = c("GPT-3.5-Turbo Pos", "GPT-3.5-Turbo Neg",
                                          "Davinci Pos", "Davinci Neg",
                                          "Curie Pos", "Curie Neg",
                                          "Curie (FT) Pos", "Curie (FT) Neg",
                                          "log(Words)", "log(BM)", "log(Turnover)",
                                          "log(Market Cap.)"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)


stargazer::stargazer(openaiReg, 
                     type = "latex", 
                     out =  "Regressions/output/table4.tex",
                     column.sep.width = "0.5",
                     font.size = "small",
                     keep.stat = c("n", "adj.rsq"),
                     dep.var.labels = "Buy-and-Hold Abnormal Return",
                     digits = 3, 
                     covariate.labels = c("GPT-3.5-Turbo Pos", "GPT-3.5-Turbo Neg",
                                          "Davinci Pos", "Davinci Neg",
                                          "Curie Pos", "Curie Neg",
                                          "Curie (FT) Pos", "Curie (FT) Neg",
                                          "log(Words)", "log(BM)", "log(Turnover)",
                                          "log(Market Cap.)"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)

#-------------------------------------------------------------------------------
# NewsWeb regression | Sub-sample split
#-------------------------------------------------------------------------------
splitReg = list()

mean(reg_meta$words)
median(reg_meta$mcap)

colnames(reg_meta)

splitReg[[1]] <- lfe::felm(
  ret_contempMkt ~ curie_pos + curie_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap >= 2000000000), psdef=FALSE)

splitReg[[2]] <- lfe::felm(
  ret_contempMkt ~ curie_pos + curie_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap < 2000000000), psdef=FALSE)

splitReg[[3]] <- lfe::felm(
  ret_contempMkt ~ curie_pos_fine_tuned + curie_neg_fine_tuned + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap >= 2000000000), psdef=FALSE)

splitReg[[4]] <- lfe::felm(
  ret_contempMkt ~ curie_pos_fine_tuned + curie_neg_fine_tuned + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap < 2000000000), psdef=FALSE)

splitReg[[5]] <- lfe::felm(
  ret_contempMkt ~ davinci_pos + davinci_neg + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap >= 2000000000), psdef=FALSE)

splitReg[[6]] <- lfe::felm(
  ret_contempMkt ~ davinci_pos + davinci_neg + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap < 2000000000), psdef=FALSE)

splitReg[[7]] <- lfe::felm(
  ret_contempMkt ~ chatgpt_pos + chatgpt_neg + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap >= 2000000000), psdef=FALSE)

splitReg[[8]] <- lfe::felm(
  ret_contempMkt ~ chatgpt_pos + chatgpt_neg + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap < 2000000000), psdef=FALSE)

splitReg[[9]] <- lfe::felm(
  ret_contempMkt ~ mnir_uni_pos + mnir_uni_neg + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap >= 2000000000), psdef=FALSE)

splitReg[[10]] <- lfe::felm(
  ret_contempMkt ~ mnir_uni_pos + mnir_uni_neg + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap < 2000000000), psdef=FALSE)

splitReg[[11]] <- lfe::felm(
  ret_contempMkt ~ mnir_bi_pos + mnir_bi_neg + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap >= 2000000000), psdef=FALSE)

splitReg[[12]] <- lfe::felm(
  ret_contempMkt ~ mnir_bi_pos + mnir_bi_neg + log(mcap) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% 
    filter(mcap < 2000000000), psdef=FALSE)


# Output for R:
stargazer::stargazer(splitReg, 
                     type = "text",
                     digits = 3, 
                     keep.stat = c("n", "rsq","adj.rsq"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*t'))

################################################################################
#                                   WSJ
################################################################################
#-------------------------------------------------------------------------------
# WSJ Regressions | Load
#-------------------------------------------------------------------------------
rm(list = ls())
meta_wsj <- readRDS("external_validity_wsj/meta_wsjRegression_v2")
#-------------------------------------------------------------------------------
# WSJ Regressions | Adjustment
#-------------------------------------------------------------------------------
reg_meta <- meta_wsj %>% 
  mutate(ret_contemp = Winsorize(ret_contemp, probs = c(0.01,0.99)),
         bm = Winsorize(bm, probs = c(0.01, 0.99)),
         turnover = Winsorize(turnover, probs = c(0.01, 0.99)), 
         mcap = Winsorize(mcap, probs = c(0.01, 0.99))  
  )

reg_meta[which(reg_meta$turnover == 0),]$turnover <- 
  reg_meta[which(reg_meta$turnover == 0), ]$turnover + 0.00000001

#-------------------------------------------------------------------------------
# WSJ Regressions | Run
#-------------------------------------------------------------------------------
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

# Fix code so that "t" is removed from the t-values
get_ts <- function(fm) {
  summary(fm)$coefficients[,3]
}
get_pvals <- function(fm) {
  summary(fm)$coefficients[,4]
}
ts <- lapply(reg, get_ts)
pvals <- lapply(reg, get_pvals)

# Output for R:
stargazer::stargazer(reg, 
                     type = "text", 
                     keep.stat = c("n", "adj.rsq"),
                     digits = 3, 
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)
saveRDS(reg, file = "Regressions/reg_lists/reg_wsj.rds")

# Main regression table output for paper (without control variables):
stargazer::stargazer(reg, 
                     type = "latex", 
                     out = "Regressions/output/table5.tex",
                     column.sep.width = "0.75",
                     font.size = "small",
                     omit = c("words", "bm", "turnover", "mcap", "(1)"),
                     keep.stat = c("n","adj.rsq"),
                     dep.var.labels = "Buy-and-Hold Abnormal Return",
                     digits = 3, 
                     covariate.labels = c("LM Positive", "LM Negative",
                                          "ML (uni) Positive", "ML (uni) Negative",
                                          "ML (bi) Positive", "ML (bi) Negative",
                                          "GPT-3.5-Turbo Positive", "GPT-3.5-Turbo Negative"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)


stargazer::stargazer(reg, 
                     type = "latex", 
                     out = "Regressions/output/table6.tex",
                     column.sep.width = "0.75",
                     font.size = "small",
                     keep.stat = c("n","adj.rsq"),
                     dep.var.labels = "Buy-and-Hold Abnormal Return",
                     digits = 3, 
                     covariate.labels = c("LM Positive", "LM Negative",
                                          "ML (uni) Positive", "ML (uni) Negative",
                                          "ML (bi) Positive", "ML (bi) Negative",
                                          "GPT-3.5-Turbo Positive", "GPT-3.5-Turbo Negative",
                                          "log(Words)", "log(BM)", "log(Turnover)",
                                          "log(Market Cap.)"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)

################################################################################
#                         NewsWeb w/ Human classification
################################################################################
#-------------------------------------------------------------------------------
# Humans vs. ML, LM, and GPT
#-------------------------------------------------------------------------------
rm(list = ls())

metaHumanTesting <- readRDS("fromR/Manual_Human_Sentiment_Test/metaHumanTesting.rds")

# Regressions:
reg_meta <- metaHumanTesting %>% 
  mutate(ret_contempMkt = Winsorize(ret_contempMkt, probs = c(0.01,0.99)),
         bm = Winsorize(bm, probs = c(0.01, 0.99)),
         turnover = Winsorize(turnover, probs = c(0.01, 0.99)), 
         mcap = Winsorize(mcap, probs = c(0.01, 0.99)))


# Add pseudo-count, as there are some volumes that are zero. (log of zero dont work)
reg_meta[which(reg_meta$turnover == 0),]$turnover <- 
  reg_meta[which(reg_meta$turnover == 0), ]$turnover + 0.00000001

reg = list()

reg[[1]] <- lfe::felm(
  ret_contempMkt ~ log(words) + log(bm) + 
    log(turnover) + log(mcap)
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[2]] <- lfe::felm(
  ret_contempMkt ~ lm_pos + lm_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[3]] <- lfe::felm(
  ret_contempMkt ~ mnir_uni_pos + mnir_uni_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[4]] <- lfe::felm(
  ret_contempMkt ~ mnir_bi_pos + mnir_bi_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[5]] <- lfe::felm(
  ret_contempMkt ~ chatgpt_pos + chatgpt_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[6]] <- lfe::felm(
  ret_contempMkt ~ FinLitHu_pos + FinLitHu_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

reg[[7]] <- lfe::felm(
  ret_contempMkt ~ NonFinLitHu_pos + NonFinLitHu_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta, psdef=FALSE)

# Fix code so that "t" is removed from the t-values
get_ts <- function(fm) {
  summary(fm)$coefficients[,3]
}
get_pvals <- function(fm) {
  summary(fm)$coefficients[,4]
}
ts <- lapply(reg, get_ts)
pvals <- lapply(reg, get_pvals)

# Output for R:
stargazer::stargazer(reg, 
                     type = "text",
                     digits = 3, 
                     keep.stat = c("n","adj.rsq"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)


# Main regression table output for paper (with control variables):
stargazer::stargazer(reg, 
                     type = "latex", 
                     out = "Regressions/output/table7.tex",
                     keep.stat = c("n","adj.rsq"),
                     column.sep.width = "0.75",
                     font.size = "small",
                     dep.var.labels = "Buy-and-Hold Abnormal Return",
                   #  omit = c("words", "bm", "turnover", "mcap", "(1)"),
                     digits = 3, 
                     covariate.labels = c("LM Positive", "LM Negative",
                                          "ML (uni) Positive", "ML (uni) Negative",
                                          "ML (bi) Positive", "ML (bi) Negative",
                                          "GPT-3.5-Turbo Positive", "GPT-3.5-Turbo Negative",
                                          "MSc. Fin. Student (Positive)", "MSc. Fin. Student (Negative)",
                                          "Preschool Teacher (Positive)", "Preschool Teacher (negative)"),
                     digits.extra = 0,
                     align = T, 
                     no.space = T,
                     report=('vc*s'), 
                     se = ts, 
                     p = pvals)

