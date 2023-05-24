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

setwd("INSERT WORKING DIRECTORY")

# API Keys (EIKON and OpenAI): 
eikonapir::set_proxy_port(9000L) # (!) Open Refinitiv Eikon Terminal before Launching API (!)  
eikonapir::set_app_id("") # <-- Insert API key  

#-------------------------------------------------------------------------------
# 2.1 Creating meta data frame from scraped dataframes
#-------------------------------------------------------------------------------
# Load data
df <- readRDS("df_inside_information.rds")                                                       # All exchanges, inside information
message_texts_list <- readRDS("message_texts_list_inside_information.rds")                       # All exchanges, inside information
df_2 <- readRDS("df_ikke_informasjonspliktig_pressemelding.rds")                                 # All exchanges, ikke informasjonspliktig pressemelding
message_texts_list_2 <- readRDS("message_texts_list_ikke_informasjonspliktig_pressemelding.rds") # All exchanges, ikke informasjonspliktig pressemelding
df_3 <- readRDS("df_annen_informasjonspliktig_info.rds")                                         # All exchanges, ADDITIONAL REGULATED INFORMATION
message_texts_list_3 <- readRDS("message_texts_list_annen_informasjonspliktig_info.rds")         # All exchanges, ADDITIONAL REGULATED INFORMATION

# As df_3 contains a lot of unnecessary documents we remove these:
df_3$text <- message_texts_list_3 
df_3 <- subset(df_3, !grepl("finansiell kalender", tolower(df_3$Meldingstittel))) # Remove financial calendar update
df_3 <- subset(df_3, !grepl("financial calendar", tolower(df_3$Meldingstittel)))  # Remove financial calendar update
df_3 <- subset(df_3, !grepl("calendar", tolower(df_3$Meldingstittel)))            # Remove financial calendar update
df_3 <- subset(df_3, !grepl("kalender", tolower(df_3$Meldingstittel)))            # Remove financial calendar update
df_3 <- subset(df_3, !grepl("invitation", tolower(df_3$Meldingstittel)))          # Remove invitation documents (these dont have any information relevant for us)
df_3 <- subset(df_3, !grepl("invitasjon", tolower(df_3$Meldingstittel)))          # Remove invitation documents (these dont have any information relevant for us)
df_3 <- subset(df_3, !grepl("present.*", tolower(df_3$Meldingstittel)))           # Remove all presentation messages
df_3 <- subset(df_3, !grepl("s??knad", tolower(df_3$Meldingstittel)))             # Remove all application messages, e.g., application to trade on the exchange
df_3 <- subset(df_3, !grepl("application", tolower(df_3$Meldingstittel)))         # Remove all application messages, e.g., application to trade on the exchange
df_3 <- subset(df_3, !grepl("announc.*", tolower(df_3$Meldingstittel)))           # Remove publication announcement, e.g., "publication of Q4 result at XX date"
df_3 <- subset(df_3, !grepl("publ.*", tolower(df_3$Meldingstittel)))              # Remove publication announcement, e.g., "publication of Q4 result at XX date"
df_3 <- subset(df_3, !grepl("notice", tolower(df_3$Meldingstittel)))              # Remove notice, E.g., notice for bondholders to attend general meeting
df_3 <- subset(df_3, !grepl("innkalling", tolower(df_3$Meldingstittel)))          # Remove notice, E.g., notice for bondholders to attend general meeting

# Merge data:
df <- rbind(df, df_2)
message_texts_list <- c(message_texts_list, message_texts_list_2)

# Create df with text:
df$text <- message_texts_list
df <- rbind(df, df_3)                                                             # Merge in df_3
df$text <- as.character(df$text)
colnames(df) <- c("index", "time", "market", "ticker", "title", "korr", "attatchments", "type", "url_code", "url", "text") 
df$korr <- NULL

# Clean environment:
rm(df_2, message_texts_list_2, df_3, message_texts_list_3, message_texts_list)

# Below I use multidplyr -- dplyr with parallel processing (more info: https://multidplyr.tidyverse.org/articles/multidplyr.html)
# Info: https://cran.r-project.org/web/packages/multidplyr/multidplyr.pdf
# Create cluster
cl <- new_cluster(detectCores() - 1)      # Clusters created with this function will automatically clean up after themselves.

# Partition df into one df per processor
df <- df %>% 
  partition(cl)

# Add library that needs to be run in cluster:
cluster_library(cl, "cld2")              # Language detection on each processor
cluster_library(cl, "stringr")           # Count words

# Clean data (Here I run dplyr on multiple cores [multidplyr]):


df <- df %>%  
  mutate(words = str_count(text,  pattern = "\\w+"),                      # Count words
         lang = detect_language(text),                                    # Find language
         time_of_day = lapply(strsplit(time, '\\ '), '[', 2),             # Time variable
         date = lapply(strsplit(time, '\\ '), '[', 1),
         
         # Raw text:
         raw_text = text, 
         
         # Text cleaning for machine learning model an LM:
         text = tolower(text),                                            # Make all lower case
         text = gsub("(http|https)://[a-zA-Z0-9./?=_-]+", "", text),      # Remove http urls
         text = gsub("www\\.[a-zA-Z0-9./?=_-]+", "", text),               # Remove www.
         text = sub("important notice.*", "", text),                      # Remove legal
         text = sub("for further information.*", "", text),               # Remove legal
         text = sub("ends.*", "", text),                                  # Remove legal
         text = sub("disclosure regulation.*", "", text),                 # Remove legal 
         text = sub("forward-looking statements.*", "", text),            # Remove forward-looking statements
         text = gsub("\\s+", " ", text),                                  # Remove excess whitespace
         text = gsub("[\n]", "", text),                                   # Remove multiple lines (paragraphs)
         text = gsub('[[:punct:]]+', '', text),                           # Remove punctuations
         text = gsub('[[:digit:]]+', '', text),                           # Remove digits
  ) %>%                               
  collect()                                                               # Collect the list into a dataframe


# Filter the dataframe and create meta:
meta <- df %>%  
  filter(lang == "en",                                                      # Select only english
         words >= 50,                                                       # Remove "noisy" docs
         words <= 1000)                                                     # OpenAI has a token limit  

wc_check <- df %>% filter(lang == "en", words >=50, words)

(nrow(wc_check) - nrow(meta))/ nrow(wc_check)

# Inspect random text:
meta$text[sample(1:nrow(meta), 1)]
meta$text[332]
meta$url[332]

# Clean date columns and time columns:
df$date <- unlist(df$date)
df$date <- as.Date(df$date, format = "%d.%m.%Y")
meta$date <- unlist(meta$date)
meta$date <- as.Date(meta$date, format = "%d.%m.%Y")
meta$year <- year(meta$date)
meta$time_of_day <- unlist(meta$time_of_day)
meta$time_of_day <- strptime(meta$time_of_day, format = "%H:%M")
meta$time_of_day <- sprintf("%02d:%02d:%02d", hour(meta$time_of_day), minute(meta$time_of_day), second(meta$time_of_day))
meta$qq_yyyy <- paste0(quarter(meta$date),"-",year(meta$date))

# Extract returns: 
# Source: Refinitiv Eikon Excel Add-in
prices <- read_excel("all_stock_prices_and_volum.xlsx", sheet = "Absolute returns")
volume <- read_excel("all_stock_prices_and_volum.xlsx", sheet = "Volume")
#industry <- read_excel("gics.xlsx", sheet = "all_industries")
mapper <- read_excel("isin_stock_binder.xlsx")

# Remove NO Equity from colnames: 
colnames(prices) <- gsub(" NO Equity", "", colnames(prices))
colnames(volume) <- gsub(" NO Equity", "", colnames(volume))
#industry$`Member Ticker` <- gsub(" NO Equity", "", industry$`Member Ticker`)
#colnames(industry) <- c("name", "ticker", "mcap", "rev", "gics")

# Make all columns numeric:
prices[,2:max(length(prices))] <- lapply(prices[,2:max(length(prices))], as.numeric)
prices$date <- as.Date(prices$date) # Fix date format
volume[,2:max(length(volume))] <- lapply(volume[,2:max(length(volume))], as.numeric)
prices$date <- as.Date(volume$date) # Fix date format

# Pivot volume from wide to long: 
volume <- volume %>% 
  pivot_longer(cols = 2:length(volume), names_to = "ticker", values_to = "volume")

# Merge volume with meta: 
meta <- left_join(meta, volume, by = c("date", "ticker"))

# Adjust dates, so that all dates fall on trading days: 
# Load OBX Trading calendar: 
load_quantlib_calendars("Norway", from = "2013-01-01", to = "2023-01-31")

# Number of non-trading dates:
nrow(meta) - sum(is.bizday(meta$date, "QuantLib/Norway"))

# Adjust meta$date so that all dates are trading days: 
meta$date <- as.Date(adjust.next(meta$date, "QuantLib/Norway"))

# Lagged date: 
meta$lag_date <- adjust.previous(meta$date - days(1), "QuantLib/Norway")

# Lead date: 
meta$lead_date <- adjust.next(meta$date + days(1), "QuantLib/Norway")

# Find tickers that are missing:
#unique(meta$ticker)
#unique(pivot_longer(prices, cols = colnames(prices[,-1]), names_to = "ticker", values_to = "price")$ticker)
#diff <- setdiff(unique(meta$ticker), unique(price$ticker))
#openxlsx::write.xlsx(data.frame(diff), file = "avlistet_tickers_1.xlsx")

#-------------------------------------------------------------------------------
# 2.2 RETURN CALCULATION (EXPECTED RETURNS AND EXCESS RETURNS) -- Monthly
#-------------------------------------------------------------------------------
# Import riskfree rate (ODEGAARD): 
rf <- read.delim("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/Rf_monthly.txt", sep = ",")
rf <- rf[-1,]
names(rf) <- c("date", "rf")

rf$date <- paste(substr(rf$date, 1, 4), "-", substr(rf$date, 5, 6), "-", substr(rf$date, 7, 8), sep = "")
rf$date <- as.Date(rf$date)

rf <- rf[order(rf$date), ]
rf$rf <- as.numeric(rf$rf)

# Import FF3 data (ODEGAARD): 
ff3 <- read.delim("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/pricing_factors_monthly.txt", sep = ",")

ff3$date <- paste(substr(ff3$date, 1, 4), "-", substr(ff3$date, 5, 6), "-", substr(ff3$date, 7, 8), sep = "")
ff3$date <- as.Date(ff3$date)

ff3 <- ff3[, c("date", "SMB", "HML")]

# Import MKT data (ODEGAARD):
mkt <- read.delim("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/market_portfolios_monthly.txt", sep = ",")

mkt$date <- paste(substr(mkt$date, 1, 4), "-", substr(mkt$date, 5, 6), "-", substr(mkt$date, 7, 8), sep = "")
mkt$date <- as.Date(mkt$date)

mkt <- mkt[, c("date", "VW")]
colnames(mkt) <- c("date", "MKT")

# Filter stock data (PRICES):
months <- seq(as.Date(min(prices$date) - days(1)), as.Date(max(prices$date)), by = "1 month") - 1 # Get last day of month

# Add ret dataframe (stock price dataframe pivoted longer)
ret <- pivot_longer(prices, cols = colnames(prices[,-1]), names_to = "securityID", values_to = "price")

ret$date.aux = cut(ret$date, months, right = T) # Auxiliary date column

interval = as.numeric(ret$date.aux)
ret$date.aux <- months[interval + 1]

ret <- ret[order(ret$securityID, ret$date), ]
ret$row <- 1:nrow(ret)

rows <- aggregate(ret$row, list(ret$date.aux, ret$securityID), max)
ret <- ret[rows$x, ]
ret$row <- NULL

# Pivot ret back to wide: 
ret <- pivot_wider(ret, names_from = "securityID", values_from = "price")

# Function for simple returns: 
simple_return <- function(x) {
  (x - dplyr::lag(x))/dplyr::lag(x)
}

# Calculate returns: 
ret <- ret[order(ret$date.aux), ]
ret <- data.frame(date = ret$date, date.aux = ret$date.aux, lapply(ret[, c(-1, -2)], simple_return))[-1, ]

# Merge:
ff3 <- merge(ff3, rf, by = "date")
ff3 <- merge(ff3, mkt, by = "date")
ret <- merge(ret, ff3, by.x = "date.aux", by.y = "date")

# Calculate excess returns over rf: 
ret[ ,!colnames(ret) %in% c("date", "date.aux" ,"rf","SMB","HML")] = ret[,!colnames(ret) %in% c("date","date.aux","rf","SMB","HML")] - ret[,"rf"]

# Make dates rownames: 
#rownames(ret) = ret[,1]
#ret = ret[,-1]

# Identify columns hat only contains NAs and remove them: (NB: IF THERE ARE STOCKS LISTED AFTER 06-2022 THERE IS NOT FF3 DATA ON IT) 
cols_with_only_nas <- which(colSums(is.na(ret)) == nrow(ret))
ret <- ret[, -cols_with_only_nas]


## FF3 Regression:
# Select variables for regressions: 
vars <- c(colnames(ret[, !colnames(ret) %in% c("date", "date.aux", "rf","SMB","HML","MKT")]))

# Function to run regression on i-th variable (I here run regressions and store them in a list: 
fit <- lapply(vars, function(x) {
  
  lm(substitute(i ~ MKT + SMB + HML, list(i = as.name(x))), data = ret)  #+ smb + hml
  
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

## Market regression: 
fit <- lapply(vars, function(x) {
  
  lm(substitute(i ~ MKT, list(i = as.name(x))), data = ret)  
  
})

beta_mat_mkt <- matrix(NA, nrow = length(vars), ncol = 4)

# For loop to store regressions in matrix:
for(i in 1:length(vars)) {
  
  # main coefficients:
  beta_mat_mkt[i,1] <- summary(fit[[i]])$coefficients[2] # mkt
  
  # Other variables
  beta_mat_mkt[i,2] <- summary(fit[[i]])$coefficients[1] # alpha
  beta_mat_mkt[i,3] <- summary(fit[[i]])$adj.r.squared   # adj. r2
  beta_mat_mkt[i,4] <- nobs(fit[[i]])                    # observations
}

# Change col and row names: 
colnames(beta_mat_mkt) <- c('MKT', "alpha", "adj.r2", "obs")
rownames(beta_mat_mkt) <- vars

# Convert to DF:
beta_df_mkt <- as.data.frame(beta_mat_mkt)
beta_df_mkt <- rownames_to_column(beta_df_mkt)
colnames(beta_df_mkt) <- c('ticker','MKT', "alpha", "adj.r2", "obs")

## Expected returns: 
rf = mean(ret$rf) * 12            # FIX this to Norwegian 10-year treasury
r_mkt = mean(ret$MKT)*12
r_hml = mean(ret$HML)*12
r_smb = mean(ret$SMB)*12

# FF3:
beta_df <- beta_df %>% 
  mutate(e_r_ff3 = MKT*r_mkt + HML*r_hml + SMB*r_smb,                        #rf have already been subtracted above
         e_r_daily_ff3 = e_r_ff3/250) %>% 
  select(ticker, e_r_daily_ff3)

# MKT: 
beta_df_mkt <- beta_df_mkt %>% 
  mutate(e_r_mkt = MKT*r_mkt,
         e_r_daily_mkt = e_r_mkt/250) %>% 
  select(ticker, e_r_daily_mkt)

# Calculate daily returns for prices: 
ret <- data.frame(date = as.Date(prices$date), lapply(prices[, -1], simple_return))[-1, ]

# Change name of colnames that are "numeric", 2020 Bulkers and 5PG. 
colnames(ret) <- c("date", "2020", "5PG", colnames(ret)[4:ncol(ret)])

#mkt_daily: 
mkt_daily <- read.delim("https://ba-odegaard.no/financial_data/ose_asset_pricing_data/market_portfolios_daily.txt", sep = ",")

mkt_daily$date <- paste(substr(mkt_daily$date, 1, 4), "-", substr(mkt_daily$date, 5, 6), "-", substr(mkt_daily$date, 7, 8), sep = "")
mkt_daily$date <- as.Date(mkt_daily$date)

mkt_daily <- mkt_daily[, c("date", "VW")]
colnames(mkt_daily) <- c("date", "mkt_daily")

# Pivot prices from wide to long: 
ret <- ret %>% 
  pivot_longer(cols = 2:length(ret), names_to = "ticker", values_to = "ret") %>% 
  left_join(beta_df, by = "ticker") %>%
  left_join(beta_df_mkt, by = "ticker") %>% 
  left_join(mkt_daily, by = "date") %>% 
  group_by(ticker) %>% 
  arrange(date) %>% 
  
  mutate(
    date = as.Date(date),
    ticker = as.character(ticker),
    #ret = scale(ret),
    raw_ret = ret,
    ret = ret - e_r_daily_ff3,
    ret_mkt = ret - e_r_daily_mkt, 
    ret_obx = ret - mkt_daily,
    
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
    ret_contemp = (1+lag_ret)*(1+ret)*(1+lead_ret)-1,
    
    # Event window returns (MKT):
    ret_contemp_mkt = (1+dplyr::lag(ret_mkt))*(1+ret_mkt)*(1+dplyr::lead(ret_mkt))-1,
    
    # Event window returns (Excess over OBX):
    ret_contemp_obx = (1+dplyr::lag(ret_obx))*(1+ret_obx)*(1+dplyr::lead(ret_obx))-1
    
  ) 

# Merge with meta: 
meta <- left_join(meta, ret, by = c("date", "ticker"))

# Remove texts without returns:
meta <- meta %>% 
  filter(!is.na(ret),
         !is.na(ret_contemp)) 

rm(prices, beta_df, beta_mat, fit, mapper, mkt, ff3, rf, rows, volume, ret, cl)

returns <- meta %>%
  select(url, ret_contemp_mkt, ret_contemp_obx) 

#-------------------------------------------------------------------------------
# 2.3 Adding control variables (Eikon API)
#-------------------------------------------------------------------------------
# Eikon Refinitiv API:
# https://community.developers.refinitiv.com/questions/37764/how-to-connect-r-with-reuters-eikon.html
# install_github("ahmedmohamedali/eikonapir")

eikonapir::set_proxy_port(9000L)
eikonapir::set_app_id("") # API Key

refinitiv_ticker <- read_excel("industry_meta.xlsx")
meta <- merge(meta, refinitiv_ticker[, c(-3,-4)], by = "ticker") 

# Define ticks: 
tics <- as.list(c(unique(meta$ticker_refinitiv)))

# Extract data from Refinitiv: 
isin <- get_symbology(tics,"RIC",list("ISIN"),raw_ouput = FALSE,debug=FALSE)
industry <- eikonapir::get_data(tics, list("TR.TRBCIndustryGroup"))

meta <- merge(meta, isin, by.x = "ticker_refinitiv", by.y = "Symbol")
colnames(industry) <- c("Instrument", "trbc_industry")
meta <- merge(meta, industry, by.x = "ticker_refinitiv", by.y = "Instrument")

# Extract eikon_data for lag_date:
eikon_data <- list()

start <- Sys.time()
for(i in 1:length(unique(meta$ticker_refinitiv)))  {
  eikon_data[[i]] <- eikonapir::get_data(as.list(unique(meta$ticker_refinitiv))[[i]][1], 
                                         list("TR.CompanyMarketCap", "TR.CompanyMarketCap.Date",  # Market cap 
                                              "TR.F.ShHoldEqCom",                                 # Shareholder equity 
                                              "TR.F.ComShrOutsTotDR",                             # Common shares outstanding
                                              "TR.Volume"),                                       # Trading volume
                                         list("Frq"="D", "SDate"="2013-01-01", "EDate" = "2023-01-31"))
  progress(i, length(unique(meta$ticker_refinitiv)))
}
Sys.time() - start

eikon_data <- lapply(eikon_data, function(x) {
  data.frame(x)
})

# Store results in dataframe:
eikon_data <- bind_rows(eikon_data)
colnames(eikon_data) <- c("ticker", "mcap", "date", "shareholder_equity", "shares_outstanding", "volume")
eikon_data$date <- as.Date(eikon_data$date)
eikon_data$mcap <- as.numeric(eikon_data$mcap)
eikon_data$shareholder_equity <- as.numeric(eikon_data$shareholder_equity)
eikon_data$shares_outstanding <- as.numeric(eikon_data$shares_outstanding)
eikon_data$volume <- as.numeric(eikon_data$volume)

# Join mcap and shareholder_equity and shareholder equity with meta:
meta <- merge(meta, data.frame(date = eikon_data$date,
                               ticker = eikon_data$ticker, 
                               mcap = eikon_data$mcap,
                               shareholder_equity = eikon_data$shareholder_equity), 
              by.x = c("ticker_refinitiv", "lag_date"), by.y = c("ticker", "date")) 

# Calculate book-to-market: 
meta$bm <- meta$shareholder_equity/meta$mcap

# Calculate average volume 50 days prior to event:
turnover <- data.frame(date = eikon_data$date,
                       ticker = as.character(eikon_data$ticker), 
                       volume = eikon_data$volume)

turnover <- na.omit(turnover)
turnover <- turnover[order(turnover$date), ]

#t <- mean(turnover[turnover[,2] == meta$ticker_refinitiv[1] & turnover[,1] <= meta$lag_date[1] & turnover[,1] >= meta$lag_date[1] - days(30), 3], na.rm = T)

for(i in 1:nrow(meta)) {
  meta$avg_vol[i] <- mean(turnover[turnover[,2] == meta$ticker_refinitiv[i] 
                                   & turnover[,1] <= meta$lag_date[i] 
                                   & turnover[,1] >= meta$lag_date[i] - days(30), 3], na.rm = T)
  
  meta$n_avg_vol[i] <- length(turnover[turnover[,2] == meta$ticker_refinitiv[i] 
                                       & turnover[,1] <= meta$lag_date[i] 
                                       & turnover[,1] >= meta$lag_date[i] - days(30), 3])
  
  progress(i, nrow(meta), progress.bar = T)
}


# Join shares_outstanding with meta:
meta <- merge(meta, data.frame(date = eikon_data$date,
                               ticker = eikon_data$ticker, 
                               shares_outstanding = eikon_data$shares_outstanding),
              by.x = c("ticker_refinitiv", "date"), by.y = c("ticker", "date")) 

# Calculate turnover:
meta$turnover = meta$avg_vol / meta$shares_outstanding

# Clean meta: 
meta$attatchments <- NULL
meta <- na.omit(meta)
meta <- meta[meta$n_avg_vol>5,]             # Remove files with less than 5 days trading volume
meta <- meta[!is.infinite(meta$turnover), ] # Remove inf values
meta <- meta[meta$bm>0, ]                   # remove negative book-to-market
meta$euronext_growth <- ifelse(meta$market %in% c("MERK", 
                                                  "XOAM, MERK", 
                                                  "XOAX, MERK", 
                                                  "XOSL, MERK", 
                                                  "XOSL, XOAX, MERK"), 1, 0) # Dummy for euronext growth/merkur
# Create training and test data: 
set.seed(1)
training <- sample(unique(meta$ticker), round(length(unique(meta$ticker))/2, 0))

meta <- meta %>% 
  mutate(group = ifelse(ticker %in% training, "training", "holdout"))

