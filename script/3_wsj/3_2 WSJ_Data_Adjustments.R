# We here adjust the meta data file as we want to replace FF3 coefficients
# excess returns over sp500

rm(list = ls())
options(scipen = 999)
Sys.setlocale("LC_CTYPE", "nb_NO.utf8")
library(tidyverse)                        # Dplyr etc. 
library(tidytext)                         # Clean text
library(readxl)                           # Load excel files
library(lubridate)                        # Manipulate dates

setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

# Calculate CAR: 
ret_wsj <- read.csv("external_validity_wsj/wsj_return.csv")
ret_wsj$date <- as.Date(ret_wsj$date)
ret_wsj <- ret_wsj[,c(2,3,7)]
ret_wsj <- pivot_wider(ret_wsj, names_from = "ticker", values_from = "prc")

simple_return <- function(x) {(x - dplyr::lag(x))/dplyr::lag(x)}
ret_wsj <- data.frame(date = ret_wsj$date, lapply(ret_wsj[, c(-1)], simple_return))[-1, ]

# Add Market index:
sp500 <- read_excel("external_validity_wsj/sp500.xlsx")
sp500$Date <- format(as.Date(sp500$Date, "%b %d, %Y"), "%Y-%m-%d")
sp500 <- tibble(date = sp500$Date, sp500 = as.numeric(gsub(",", "", sp500$Close)))
sp500 <- sp500[order(sp500$date),]
sp500 <- data.frame(date = as.Date(sp500$date), lapply(sp500[, "sp500"], simple_return))

ret_wsj <- left_join(ret_wsj, sp500, by = "date")

# Calculate excess returns over sp500: 
ret_wsj[ ,!colnames(ret_wsj) %in% c("date", "sp500")] <- 
  ret_wsj[,!colnames(ret_wsj) %in% c("date", "sp500")] - ret_wsj[,"sp500"]

ret_wsj$sp500 <- NULL 
ret_wsj <- pivot_longer(ret_wsj, cols = -1, names_to = "ticker", values_to = "ret")

ret_wsj <- ret_wsj %>%
  group_by(ticker) %>% 
  arrange(date) %>% 
  mutate(lag_ret = dplyr::lag(ret),
         lead_ret = dplyr::lead(ret),
         ret_contemp = (1+lag_ret)*(1+ret)*(1+lead_ret)-1) %>%  
  select(ticker, date, ret, lag_ret, lead_ret, ret_contemp)

# Merge with meta_wsj:
meta_wsj <- readRDS("external_validity_wsj/meta_wsj_final.rds")
meta_wsj <- meta_wsj[,-c(24:27)]
meta_wsj <- left_join(meta_wsj, ret_wsj, by = c("date", "ticker"))

saveRDS(meta_wsj, file = "external_validity_wsj/meta_wsjAdj.rds")



