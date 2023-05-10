################################################################################
#                         Create External Validity Data              
################################################################################

rm(list = ls())
options(scipen = 999)
Sys.setlocale("LC_CTYPE", "nb_NO.utf8")
library(eikonapir)                        # Refinitiv Eikon API for R
library(svMisc)                           # Progress tracker
library(xml2)
library(tidyverse)                        # Dplyr etc. 
library(tidytext)                         # Clean text
library(textmineR)                        # DTM and TF-IDF filtering
library(tm)                               # Textmining (incl. stopwords)
library(parallel)                         # Parallel computing
library(readxl)                           # Load excel files
library(lubridate)                        # Manipulate dates
library(PerformanceAnalytics)             # Calculate returns etc. 
library(quantmod)                         # Stopwords, textual analysis etc.
library(quanteda)                         # Text analysis
library(data.table)
library(rrapply)                           
library(kableExtra)
library(multidplyr)                       # Dplyr with parallel 
library(openxlsx)                         # Save and read .xlsx
library(bizdays)                          # Business days 
library(rworldmap)
library(remotes)
library(stringr)
library(slam)
library(SentimentAnalysis)           
library(TTR)

setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

# API Keys (EIKON and OpenAI): 
eikonapir::set_proxy_port(9000L) # (!) Open Refinitiv Eikon Terminal before Launching API (!)  
eikonapir::set_app_id("c4b851b15e7d40529ee9cb8c562e87ffa5ff0d57")               

#-------------------------------------------------------------------------------
# 3.1 Create external validity data 
#-------------------------------------------------------------------------------
meta_wsj <- readRDS("external_validity_wsj/meta_wsj.Rdata")
meta_wsj$id <- seq(from = 1, to = nrow(meta_wsj), by = 1)
tickers_wsj <- read_excel("external_validity_wsj/tickers_wsj.xlsx")
tickers_wsj <- tickers_wsj %>% 
  filter(isin != "NULL")
meta_wsj <- merge(meta_wsj, tickers_wsj, by = "ticker")
meta_wsj$ret <- NULL

# Calculate CAR: 
ret_wsj <- read.csv("external_validity_wsj/wsj_return.csv")
ret_wsj$date <- as.Date(ret_wsj$date)
ret_wsj <- ret_wsj[,c(2,3,7)]
ret_wsj <- pivot_wider(ret_wsj, names_from = "ticker", values_from = "prc")

simple_return <- function(x) {(x - dplyr::lag(x))/dplyr::lag(x)}
ret_wsj <- data.frame(date = ret_wsj$date, lapply(ret_wsj[, c(-1)], simple_return))[-1, ]

head(ret_wsj)

# Add FF3:
ff3 <- read.csv("external_validity_wsj/F-F_Research_Data_Factors_daily.csv", sep = ",")
ff3[,2:length(ff3)] <- as.numeric(unlist(ff3[,2:length(ff3)]))
ff3[,2:length(ff3)] <- ff3[,2:length(ff3)]/100
ff3$date <- paste(substr(ff3$date, 1, 4), "-", substr(ff3$date, 5, 6), "-", substr(ff3$date, 7, 8), sep = "")
ff3$date <- as.Date(ff3$date)

ret_wsj <- merge(ret_wsj, ff3, by = "date")

# Calculate excess returns over rf: 
ret_wsj[ ,!colnames(ret_wsj) %in% c("date", "Mkt.RF", "RF","SMB","HML")] <- 
  ret_wsj[,!colnames(ret_wsj) %in% c("date", "Mkt.RF", "RF","SMB","HML")] - ret_wsj[,"RF"]

## FF3 Regression:
#Remove NA rows:
#ret_wsj <- ret_wsj[, -which(colSums(is.na(ret_wsj)) == nrow(ret_wsj))]

# Select variables for regressions: 
vars <- c(colnames(ret_wsj[, !colnames(ret_wsj) %in% c("date", "Mkt.RF", "RF","SMB","HML")]))

# Function to run regression on i-th variable (I here run regressions and store them in a list: 
fit <- lapply(vars, function(x) {
  lm(substitute(i ~ Mkt.RF + SMB + HML, list(i = as.name(x))), data = ret_wsj)
})

# Empty matrix to store regression result: 
beta_mat <- matrix(NA, nrow = length(vars), ncol = 6)

# For loop to store regressions in matrix:
for(i in 1:length(vars)) {
  
  # main coefficients:
  beta_mat[i,1] <- summary(fit[[i]])$coefficients[2] # mkt
  beta_mat[i,2] <- summary(fit[[i]])$coefficients[3] # smb
  beta_mat[i,3] <- summary(fit[[i]])$coefficients[4] # hml
  
  # Other variables
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
rf = mean(ret_wsj$RF)
r_mkt = mean(ret_wsj$Mkt.RF)
r_hml = mean(ret_wsj$HML)
r_smb = mean(ret_wsj$SMB)

beta_df <- beta_df %>% 
  mutate(eR = MKT*r_mkt + HML*r_hml + SMB*r_smb) %>% 
  select(ticker, eR)

ret_wsj <- pivot_longer(ret_wsj, cols = -1, names_to = "ticker", values_to = "return")
ret_wsj <- merge(ret_wsj, beta_df, by = "ticker")
ret_wsj <- ret_wsj %>% 
  group_by(ticker) %>% 
  arrange(date) %>% 
  mutate(ret = return - eR,
         lag_ret = dplyr::lag(return) - eR,
         lead_ret = dplyr::lead(return) - eR,
         ret_contemp = (1+lag_ret)*(1+ret)*(1+lead_ret)-1) %>%  
  select(ticker, date, ret, lag_ret, lead_ret, ret_contemp)

meta_wsj <- left_join(meta_wsj, ret_wsj, by = c("date", "ticker"))

# Get control variables from Eikon:
eikon_data <- list()
for(i in 1:length(unique(meta_wsj$ticker_refinitiv)))  {
  eikon_data[[i]] <- eikonapir::get_data(as.list(unique(meta_wsj$ticker_refinitiv))[[i]][1], 
                                         list("TR.CompanyMarketCap", "TR.CompanyMarketCap.Date",  # Market cap 
                                              "TR.F.ShHoldEqCom",                                 # Shareholder equity 
                                              "TR.F.ComShrOutsTotDR",                             # Common shares outstanding
                                              "TR.Volume",                                        # Trading volume
                                              "TR.TRBCIndustryGroup"),                            # IndustryGroup 
                                         list("Frq"="D", 
                                              "SDate" = min(meta_wsj$date), 
                                              "EDate" = max(meta_wsj$date)))
  progress(i, length(unique(meta_wsj$ticker_refinitiv)))
}

eikon_data <- lapply(eikon_data, function(x) {
  data.frame(x)
})

# Store results in dataframe:
eikon_data <- bind_rows(eikon_data)
industry <- data.frame(instrument = eikon_data$Instrument, industry = eikon_data$TRBC.Industry.Group.Name)
industry <- industry %>% 
  filter(industry != "")
meta_wsj <- merge(meta_wsj, industry, by.x = "ticker_refinitiv", by.y = "instrument")

colnames(eikon_data) <- c("ticker", "mcap", "date", "shareholder_equity", "shares_outstanding", "volume")
eikon_data$date <- as.Date(eikon_data$date)
eikon_data$mcap <- as.numeric(eikon_data$mcap)
eikon_data$shareholder_equity <- as.numeric(eikon_data$shareholder_equity)
eikon_data$shares_outstanding <- as.numeric(eikon_data$shares_outstanding)
eikon_data$volume <- as.numeric(eikon_data$volume)
eikon_data <- eikon_data[,1:6]

meta_wsj$lag_date <- adjust.previous(meta_wsj$date - days(1), "QuantLib/UnitedStates/NYSE")
meta_wsj$lead_date <- adjust.next(meta_wsj$date + days(1), "QuantLib/UnitedStates/NYSE")

# Join mcap and shareholder_equity and shareholder equity with meta:
meta_wsj <- merge(meta_wsj, data.frame(date = eikon_data$date,
                                       ticker = eikon_data$ticker,
                                       mcap = eikon_data$mcap,
                                       shareholder_equity = eikon_data$shareholder_equity),
                  by.x = c("ticker_refinitiv", "lag_date"), 
                  by.y = c("ticker", "date")) 

# Calculate book-to-market: 
meta_wsj$bm <- meta_wsj$shareholder_equity/meta_wsj$mcap

# Calculate average volume 50 days prior to event:
turnover <- data.frame(date = eikon_data$date,
                       ticker = as.character(eikon_data$ticker), 
                       volume = eikon_data$volume)

turnover <- na.omit(turnover)
turnover <- turnover[order(turnover$date), ]

#t <- mean(turnover[turnover[,2] == meta$ticker_refinitiv[1] & turnover[,1] <= meta$lag_date[1] & turnover[,1] >= meta$lag_date[1] - days(30), 3], na.rm = T)

for(i in 1:nrow(meta_wsj)) {
  meta_wsj$avg_vol[i] <- mean(turnover[turnover[,2] == meta_wsj$ticker_refinitiv[i] 
                                       & turnover[,1] <= meta_wsj$lag_date[i] 
                                       & turnover[,1] >= meta_wsj$lag_date[i] - days(30), 3], na.rm = T)
  
  meta_wsj$n_avg_vol[i] <- length(turnover[turnover[,2] == meta_wsj$ticker_refinitiv[i] 
                                           & turnover[,1] <= meta_wsj$lag_date[i] 
                                           & turnover[,1] >= meta_wsj$lag_date[i] - days(30), 3])
  
  progress(i, nrow(meta_wsj), progress.bar = T)
}


# Join shares_outstanding with meta:
meta_wsj <- merge(meta_wsj, data.frame(date = eikon_data$date,
                                       ticker = eikon_data$ticker, 
                                       shares_outstanding = eikon_data$shares_outstanding),
                  by.x = c("ticker_refinitiv", "date"), by.y = c("ticker", "date")) 

# Calculate turnover:
meta_wsj$turnover = meta_wsj$avg_vol / meta_wsj$shares_outstanding

# Clean meta: 
meta_wsj$attatchments <- NULL
meta_wsj <- na.omit(meta_wsj)
meta_wsj <- meta_wsj[meta_wsj$n_avg_vol>5,]             # Remove files with less than 5 days trading volume
meta_wsj <- meta_wsj[!is.infinite(meta_wsj$turnover), ] # Remove inf values
meta_wsj <- meta_wsj[meta_wsj$bm>0, ]                   # remove negative book-to-market
meta_wsj <- meta_wsj %>% 
  filter(text != "character(0)")

cl <- new_cluster(detectCores() - 1)      # Clusters created with this function will automatically clean up after themselves.
meta_wsj <- meta_wsj %>%                  # Partition df into one df per processor
  partition(cl)

# Add library that needs to be run in cluster:
cluster_library(cl, "stringr")           # Count words

# Clean data (Here I run dplyr on multiple cores [multidplyr]):
meta_wsj <- meta_wsj %>%  
  mutate(# Raw text:
    raw_text = text, 
    
    # Text cleaning for machine learning model an LM:
    text = tolower(text),                                            # Make all lower case
    text = gsub("www\\.[a-zA-Z0-9./?=_-]+", "", text),               # Remove www.
    text = gsub("\\s+", " ", text),                                  # Remove excess whitespace
    text = gsub("[\n]", "", text),                                   # Remove multiple lines (paragraphs)
    text = gsub('[[:punct:]]+', '', text),                           # Remove punctuations
    text = gsub('[[:digit:]]+', '', text),                           # Remove digits
  ) %>%                               
  collect()                                                               # Collect the list into a dataframe

meta_wsj$qq_yyyy <- paste0(quarter(meta_wsj$date),"-",year(meta_wsj$date))

#saveRDS(meta_wsj, file = "external_validity_wsj/meta_wsj.rds")
#saveRDS(meta_wsj, file = "external_validity_wsj/meta_wsj_index_id.rds")
