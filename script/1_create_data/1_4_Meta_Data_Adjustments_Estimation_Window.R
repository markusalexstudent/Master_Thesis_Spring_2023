# We here adjust the meta data file as we want to replace monthly FF3 coefficients
# with daily FF3 coefficients as this was recommended by our supervisor

rm(list = ls())
options(scipen = 999)
Sys.setlocale("LC_CTYPE", "nb_NO.utf8")

library(eikonapir)                        # Refinitiv Eikon API for R
library(RSelenium)                        # Docker and Selenium
library(rvest)                            # Webscraping
library(svMisc)                           # Progress tracker
library(xml2)
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
library(data.table)
library(textir)                           # MNIR ML Model
library(rrapply)                           
library(kableExtra)
library(vader)                            # Sentiment ML Model
library(multidplyr)                       # Dplyr with parallel 
library(openxlsx)                         # Save and read .xlsx
library(bizdays)                          # Business days 
library(reticulate)                       # Run Python in R
library(rworldmap)
library(remotes)
library(stringr)
library(slam)
library(SentimentAnalysis)           
library(TTR)
library(plm)                              # Panel data
library(wordcloud)                        # Wordcloud
library(wordcloud2)                       # Wordcloud2
library(jsonlite)                         # Create json file for fine tuning
library(openai)                           # OpenAI for fine-tuning
library(DescTools)                        # Winsorize
library(wordcloud2)                       # devtools::install_github("lchiffon/wordcloud2")

setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

# Read data: 
df <- readRDS("fromR/df_meta.rds")
meta <- readRDS("fromR/meta.rds")
write_delim(meta, file = "post_filtered_data.txt")
#-------------------------------------------------------------------------------
# Fama French Three Factor Model
#-------------------------------------------------------------------------------
# Import FF3 data from Odegaard 
rf <- read.delim("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/Rf_daily.txt", sep = ",")
rf <- rf[-1,]
names(rf) <- c("date", "rf")
rf$date <- paste(substr(rf$date, 1, 4), "-", substr(rf$date, 5, 6), "-", substr(rf$date, 7, 8), sep = "")
rf$date <- as.Date(rf$date)
rf <- rf[order(rf$date), ]
rf$rf <- as.numeric(rf$rf)
rfForLater <- rf

ff3 <- read.delim("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/pricing_factors_daily.txt", sep = ",")
ff3$date <- paste(substr(ff3$date, 1, 4), "-", substr(ff3$date, 5, 6), "-", substr(ff3$date, 7, 8), sep = "")
ff3$date <- as.Date(ff3$date)
ff3 <- ff3[, c("date", "SMB", "HML")]

mkt <- read.delim("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/market_portfolios_daily.txt", sep = ",")
mkt$date <- paste(substr(mkt$date, 1, 4), "-", substr(mkt$date, 5, 6), "-", substr(mkt$date, 7, 8), sep = "")
mkt$date <- as.Date(mkt$date)
mkt <- mkt[, c("date", "VW")]
colnames(mkt) <- c("date", "MKT")

# Import stock returns from Bloomberg: 
prices <- read_excel("all_stock_prices_and_volum.xlsx", sheet = "Absolute returns")
colnames(prices) <- gsub(" NO Equity", "", colnames(prices))
prices[,2:max(length(prices))] <- lapply(prices[,2:max(length(prices))], as.numeric)
prices$date <- as.Date(prices$date) # Fix date format
colnames(prices) <- c("date", "2020", "5PG", colnames(prices)[4:ncol(prices)]) # Change name of numeric colnames

ret <- prices
simple_return <- function(x) {(x - dplyr::lag(x))/dplyr::lag(x)}
ret <- ret[order(ret$date), ]
ret <- data.frame(date = ret$date, lapply(ret[, c(-1, -2)], simple_return))[-1, ]
retForLater <- ret

# Merge FF3, Rf, Mkt, and Ret:
ff3 <- merge(ff3, rf, by = "date")
ff3 <- merge(ff3, mkt, by = "date")
ff3 <- ff3[ff3$date >= min(ret$date), ]

# Identify dates that are in ret but not in ff3:
anti_ret <- anti_join(ret, ff3, by = "date")

# Identify dates that are in ff3 but not in ret: 
anti_ff3 <- anti_join(ff3, ret, by = "date")

ret <- merge(ret, ff3, by = "date", all = TRUE)

# Calculate excess returns over rf: 
ret[ ,!colnames(ret) %in% c("date", "rf","SMB","HML")] = 
  ret[,!colnames(ret) %in% c("date","rf","SMB","HML")] - ret[,"rf"]

# Add estimation window:
ret <- pivot_longer(ret,2:327, names_to = "ticker")
colnames(ret) <- c("date", "SMB", "HML", "rf", "MKT", "ticker", "return")
eventDate <- data.frame(ticker = meta$ticker, date = meta$date, event = 1)

ret <- left_join(ret, eventDate, by = c("ticker", "date"))
ret$event[is.na(ret$event)] <- 0 

ret <- ret[order(ret$ticker), ]




cols_with_only_nas <- which(colSums(is.na(ret)) == nrow(ret))
ret <- ret[, -cols_with_only_nas]

# Regression:
vars <- c(colnames(ret[, !colnames(ret) %in% c("date", "rf", "SMB", "HML", "MKT")]))
beta_mat <- matrix(NA, nrow = length(vars), ncol = 6)

fit <- lapply(vars, function(x) {
  lm(substitute(i ~ MKT + SMB + HML, list(i = as.name(x))), data = ret)  #+ smb + hml
})

for(i in 1:length(vars)) {
  beta_mat[i,1] <- summary(fit[[i]])$coefficients[2] # mkt
  beta_mat[i,2] <- summary(fit[[i]])$coefficients[3] # smb
  beta_mat[i,3] <- summary(fit[[i]])$coefficients[4] # hml
  beta_mat[i,4] <- summary(fit[[i]])$coefficients[4] # alpha
  beta_mat[i,5] <- summary(fit[[i]])$adj.r.squared   # adj. r2
  beta_mat[i,6] <- nobs(fit[[i]])                    # observations
}

# Change col and row names: 
colnames(beta_mat) <- c('MKT', "SMB", "HML", "alpha", "adj.r2", "obs")
rownames(beta_mat) <- vars

# Convert to DF:
beta_df <- as.data.frame(beta_mat)
beta_df <- rownames_to_column(beta_df)
colnames(beta_df) <- c('ticker','MKT', "SMB", "HML", "alpha", "adj.r2", "obs")

## Expected returns: 
rf = mean(ret$rf, na.rm = T) 
r_mkt = mean(ret$MKT, na.rm = T)
r_hml = mean(ret$HML, na.rm = T)
r_smb = mean(ret$SMB, na.rm = T)

beta_df <- beta_df %>% 
  mutate(eR = MKT*r_mkt + HML*r_hml + SMB*r_smb) %>% 
  select(ticker, eR)

# Import prices on new
ret <- retForLater

ret <- ret %>% 
  pivot_longer(cols = 2:length(ret), names_to = "ticker", values_to = "ret") %>% 
  left_join(beta_df, by = "ticker") %>%
  group_by(ticker) %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date),
    ticker = as.character(ticker),
    raw_ret = ret,
    ret = ret - eR,
    lead_ret = dplyr::lead(ret),
    lag_ret = dplyr::lag(ret),
    lag_ret2 = dplyr::lag(ret, 2),
    lead_ret2 = dplyr::lead(ret, 2),
    lead_ret3 = dplyr::lead(ret, 3),
    lead_ret4 = dplyr::lead(ret, 4),
    lead_ret5 = dplyr::lead(ret, 5),
    lag_ret3 = dplyr::lag(ret, 3),
    lag_ret4 = dplyr::lag(ret, 4),
    lag_ret5 = dplyr::lag(ret, 5),
    
    # Event window returns (FF3):  
    ret_contemp = (1+lag_ret)*(1+ret)*(1+lead_ret)-1
    )

# Remove old (monthly return calculations) return numbers. 
colnames(meta)[21:34]
meta <- meta[, -c(21:34)]
colnames(meta)

# Merge with meta: 
meta <- left_join(meta, ret, by = c("date", "ticker"))

# count number of rows with NA values
sum(!complete.cases(meta))

#saveRDS(meta, file = "fromR/metaAdj.rds")

#-------------------------------------------------------------------------------
# CAPM with only MKT factor
#-------------------------------------------------------------------------------
ret <- retForLater
rf <- rfForLater
ret <- merge(ret, rf, by = "date")
ret <- merge(ret, mkt, by = "date")

# Calculate excess returns over rf: 
ret[ ,!colnames(ret) %in% c("date", "rf")] = 
  ret[,!colnames(ret) %in% c("date","rf")] - ret[,"rf"]

# Remove columns with only NAs: 
cols_with_only_nas <- which(colSums(is.na(ret)) == nrow(ret))
ret <- ret[, -cols_with_only_nas]

# Regression:
vars <- c(colnames(ret[, !colnames(ret) %in% c("date", "rf", "MKT")]))
beta_mat <- matrix(NA, nrow = length(vars), ncol = 4)

fit <- lapply(vars, function(x) {
  lm(substitute(i ~ MKT, list(i = as.name(x))), data = ret)  
})

for(i in 1:length(vars)) {
  beta_mat[i,1] <- summary(fit[[i]])$coefficients[2] # mkt
  beta_mat[i,2] <- summary(fit[[i]])$coefficients[1] # alpha
  beta_mat[i,3] <- summary(fit[[i]])$adj.r.squared   # adj. r2
  beta_mat[i,4] <- nobs(fit[[i]])                    # observations
}

# Change col and row names: 
colnames(beta_mat) <- c('MKT', "Alpha", "adj.r2", "obs")
rownames(beta_mat) <- vars

# Convert to DF:
beta_df <- as.data.frame(beta_mat)
beta_df <- rownames_to_column(beta_df)
colnames(beta_df) <- c('ticker', 'MKT', "Alpha", "adj.r2", "obs")

# Expected returns: 
rf = mean(ret$rf, na.rm = T) 
mkt.rf = mean(ret$MKT, na.rm = T)

beta_df <- beta_df %>% 
  mutate(eR = MKT*mkt.rf) %>% 
  select(ticker, eR)

# Import prices on new
ret <- retForLater

ret <- ret %>% 
  pivot_longer(cols = 2:length(ret), names_to = "ticker", values_to = "ret") %>% 
  left_join(beta_df, by = "ticker") %>%
  group_by(ticker) %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date),
    ticker = as.character(ticker),
    raw_ret = ret,
    retCapm = ret - eR,
    lead_retCapm = dplyr::lead(ret),
    lag_retCapm = dplyr::lag(ret),

    # Event window returns (FF3):  
    ret_contempCapm = (1+lag_retCapm)*(1+retCapm)*(1+lead_retCapm)-1
  ) %>% 
  select(date, ticker, retCapm, lead_retCapm, lag_retCapm, ret_contempCapm)

# Merge with meta: 
meta <- left_join(meta, ret, by = c("date", "ticker"))

# count number of rows with NA values
sum(!complete.cases(meta))

#-------------------------------------------------------------------------------
# Excess returns over market index
#-------------------------------------------------------------------------------
ret <- retForLater
osebx <- read_excel("osebx.xlsx")
colnames(osebx) <- c("date", "close", "net", "mkt", "open", "low", "high", "volume", "turnover", "flow")
osebx$date <- as.Date(osebx$date)
osebx <- osebx[,c("date", "mkt")]
ret <- ret[ret$date >= "2012-12-01", ]
ret <- merge(ret, osebx, by = "date")
ret <- pivot_longer(ret, cols = 2:(length(ret)-1), names_to = "ticker", values_to = "ret")

ret <- ret %>% 
  group_by(ticker) %>% 
  arrange(date) %>% 
  mutate(
    #date = as.Date(date),
    #ticker = as.character(ticker),
    retMkt = ret - mkt,
    lead_retMkt = dplyr::lead(retMkt),
    lag_retMkt = dplyr::lag(retMkt),
    
    # Event window returns (FF3):  
    ret_contempMkt = (1+lag_retMkt)*(1+retMkt)*(1+lead_retMkt)-1) %>% 
  select(date, ticker, retMkt, lead_retMkt, lag_retMkt, ret_contempMkt)
  
meta <- merge(meta, ret, by = c("date", "ticker"))

# count number of rows with NA values
sum(!complete.cases(meta))

#saveRDS(meta, file = "fromR/metaAdj_v3.rds")

