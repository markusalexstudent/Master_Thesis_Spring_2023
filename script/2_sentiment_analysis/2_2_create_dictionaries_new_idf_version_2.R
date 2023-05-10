################################################################################
#               CONSTRUCT DICTIONARIES & CALCULATE SENTIMENT SCORES                          
################################################################################
# Preamble, setting working directory
rm(list = ls())
setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

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
mlUni <- read_csv("robustMNIR/idf_version/final/ML_score_unigram.csv") #run_19042023_all-three-filters

# Positive ML Unigram dictionary: 
mlUniPos <- mlUni %>% 
  filter(positive-negative >= 0.6, #prev value = 0.4
         freq < 2000,
         idf < 7
         )

mlUniPos <- subset(mlUniPos, word != "igaming")
mlUniPos <- subset(mlUniPos, word != "scanship")
mlUniPos <- subset(mlUniPos, word != "easysmart")
mlUniPos <- subset(mlUniPos, word != "laptop")
mlUniPos <- subset(mlUniPos, word != "bjarnsholt")

head(mlUniPos)

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

mlUniNeg <- subset(mlUniNeg, word != "telenor")
mlUniNeg <- subset(mlUniNeg, word != "orkla")
mlUniNeg <- subset(mlUniNeg, word != "cest")
mlUniNeg <- subset(mlUniNeg, word != "sas")
mlUniNeg <- subset(mlUniNeg, word != "dof")
mlUniNeg <- subset(mlUniNeg, word != "eriksen")
mlUniNeg <- subset(mlUniNeg, word != "gate")
mlUniNeg <- subset(mlUniNeg, word != "bakkafrost")
mlUniNeg <- subset(mlUniNeg, word != "asteks")
mlUniNeg <- subset(mlUniNeg, word != "georgina")
mlUniNeg <- subset(mlUniNeg, word != "profit")

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
mlBi <- read_csv("robustMNIR/idf_version/final/ML_score_bigram.csv")

# Positive ML Bigram dictionary: 
mlBiPos <- mlBi %>% 
  filter(positive-negative >= 0.45, #0.4 for 500_it 7idf BI
         freq <= 900,
         #idf <= 8
         )

mlBiPos <- subset(mlBiPos, !word %in% c("mobile_email", "announced_today", "western_bulk", "l??vestam_zwipe",
                                        "zwipe_pay", "e_skajem", "akva_group", "scanship_processes",
                                        "scanship_provides", "residuals_scanship", "taiwan_korea",
                                        "university_copenhagen", "holding_scanship", 
                                        "holding_tel", "cleaner_oceans", "bb_clients", 
                                        "shipping_rasmussengruppen", "aker_solutions", 
                                        "ose_plt", "solutions_softox", "crayon_help", 
                                        "inc_nio", "plt_today", "heatcube_thermal", 
                                        "semisubmersible_drilling", "semisubmersible_drilling",
                                        "gamborg_andreassen", "q_adjusted", 
                                        "offices_france", "tokyo_japan",
                                        "laptop", "contact_hofshagen",  "crayon_group",
                                        "polight_polight", "platform_drilling", 
                                        "skajem_jeshuddlestockcom", "scanship_holding",
                                        "l??vestam_zwipe", "iso_iso", "innovation_group",
                                        "date_wednesday", "health_care", "magnushofshagencrayoncom_crayon",
                                        "crayon_takle", "softox_technology",
                                        "l??vestam_zwipe", "elliptic_labs",
                                        "selected_zwipe"))


# Save dictionary:
mlBiPos %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_positive_bigram.txt")
saveRDS(mlBiPos, file = "robustMNIR/mlBiPos.rds")

# Negative ML Bigram dictionary: 
mlBiNeg <- mlBi %>% 
  filter(negative-positive >= 0.45,  #0.4 for 500_it 7idf BI
         freq <= 900,
        # idf <= 8,
         ) 

mlBiNeg <- subset(mlBiNeg, !word %in% c("novel_immunotherapies", "argentine_egypt",
                                        "brazil_argentina", "awilco_drillings", 
                                        "nyse_ose", "email_presentation", 
                                        "dronning_eufemias", "eufemias_gate",
                                        "container_ships", "mpc_container", 
                                        "lse_zen", "favre_ritufavrenextbiometricscom", 
                                        "konferansesenter_bryggetorget", 
                                        "waage_basili", "gross_margin",
                                        "energy_zenith", "k_arnet",
                                        "indirectly_japan", "sparebank_nordnorge",
                                        "zenith_company", "seabird_exploration",
                                        "nordic_nanovector", "prosafe_se",
                                        "larnaca_cyprus", "zename_zeniths",
                                        "exchange_zename", "larnaca_georgina",
                                        "prosafe_worlds", "lsezen_merkur",
                                        "vesting_schedule", "tel_cfo",
                                        "awilco_drilling", "tel_multinational", 
                                        "today_cest", "andr??_sloth",
                                        "dam_madsen", "dof_international",
                                        "dof_listed", "dof_international",
                                        "dofs_core", "modern_offshoresubsea", 
                                        "singapore_brazil","cfo_dronen",
                                        "dof_offers", "psv_charter", 
                                        "ose_nanov", "bermuda_borr",
                                        "approximately_cad", "zenith_int",
                                        "placement_zenith", "cfo_dam",
                                        "meeting_dof", "beggining_one",
                                        "date_srs"))
                                        
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

#-------------------------------------------------------------------------------
# Regression check
#-------------------------------------------------------------------------------
# Winsorize:
reg_meta <- meta %>% 
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
  ret_contempMkt ~ lm_pos + lm_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% filter(group == "holdout"), psdef=FALSE)

reg[[3]] <- lfe::felm(
  ret_contempMkt ~ mnir_uni_pos + mnir_uni_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% filter(group == "holdout"), psdef=FALSE)

reg[[4]] <- lfe::felm(
  ret_contempMkt ~ mnir_bi_pos + mnir_bi_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% filter(group == "holdout"), psdef=FALSE)

reg[[5]] <- lfe::felm(
  ret_contempMkt ~ ml_lm_pos + ml_lm_neg + log(words) + log(bm) + 
    log(turnover) + log(mcap)  
  | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
  data = reg_meta %>% filter(group == "holdout"), psdef=FALSE)

stargazer::stargazer(reg, type = "text")

