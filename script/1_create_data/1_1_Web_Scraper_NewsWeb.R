rm(list = ls())
options(scipen = 999)
Sys.setlocale("LC_CTYPE", "nb_NO.utf8")

# Packages: 
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

# Docker: 
# Installation: https://www.docker.com/

# https://stackoverflow.com/questions/53088983/web-scraping-with-r-message-that-javascript-is-disabled
# Source for parallel computing: https://appsilon.com/webscraping-dynamic-websites-with-r/

#-------------------------------------------------------------------------------
# 1.1 Start Docker for web scraping
#-------------------------------------------------------------------------------
# Open Docker:
system("start docker")

# Start new container on Docker:
system('docker run -d -p 4445:4444 selenium/standalone-firefox')

remDr <-  RSelenium::remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

#-------------------------------------------------------------------------------
# 1.2 Scrape URL + meta data
#-------------------------------------------------------------------------------
#Start the remote driver
remDr$open()

# List elemts to iterate with parallel processing: 
category <- "INSERT CATEGORY TO SCRAPE HERE, E.G. '1005'"
urls_list <- c()
dates <- seq(as.Date("2013-01-01"), as.Date("2023-01-31"), by = "1 day")

for(i in 1:length(dates[-1])) {
  urls_list[i] <- paste0("https://newsweb.oslobors.no/search?category=",category,
                         "&issuer=&fromDate=",dates[i],
                         "&toDate=", dates[i],
                         "&market=&messageTitle=") #dates[-length(dates)][i]
  print(urls_list[i])
}

# Create list to store dataframes in:
df_list <- list()
message_url_list <- list()

start = Sys.time()

# Extract meta data for all links in url list:
for(i in seq_along(urls_list)) {
  
  remDr$navigate(urls_list[[i]])
  
  Sys.sleep(1) # This is added so that there is time to load each page in FireFox
  
  doc <- read_html(remDr$getPageSource()[[1]])
  
  # Extract df: 
  df_list[[i]] <- doc %>%
    html_table() %>% 
    data.frame()
  
  # Extract links to text:
  message_url_list[[i]] <- doc %>% 
    html_nodes("a") %>% html_attr('href') %>% 
    data.frame() 
  
  #print(message_url_list[[i]])
  
  progress(i, length(urls_list)) # Track progress of loop    
}

# Remove the NULL elements from the list
df_list <- Filter(function(x) nrow(x) > 0, df_list)

# Merge dataframes within each list:
df <- bind_rows(df_list)
message_urls <- bind_rows(message_url_list)

# Clean data:
message_urls <- data.frame(message_urls[grep("message", message_urls$.),])
colnames(message_urls) <- "message"

# Merge dataframes:
df$index = seq(1:nrow(df))
message_urls$index = seq(1:nrow(df))
df <- merge(df, message_urls, by = "index")

# Create an url column:
df <- df %>% 
  mutate(url_complete = paste0("https://newsweb.oslobors.no", message))

# Save df: 
# saveRDS(df, file = "df_inside_information.rds")
# df <- readRDS("df.rds")


#-------------------------------------------------------------------------------
# 1.3 Text scraping
#-------------------------------------------------------------------------------

# Create list to store text in:
message_texts_list <- list()

# Loop through each url: 
for (i in seq_along(df$url_complete)) {
  
  remDr$navigate(df$url_complete[i])
  
  Sys.sleep(1) # This is added so that there is time to load each page in FireFox
  
  docs <- read_html(remDr$getPageSource()[[1]])
  
  message_texts <- docs %>% 
    html_nodes(xpath = '/html/body/div/div/main/div[2]/div[2]/div[2]/div/text()') %>% 
    html_text()
  
  message_texts_list[[i]] <- message_texts
  
  #print(message_texts) # Print text to check if errors/missing values occour
  
  progress(i, length(df$url_complete)) # Track progress of loop
  
}

# saveRDS(message_texts_list, file = "message_texts_list_inside_information.rds")


# Close docker an Selenium:
remDr$close()
gc()
system("taskkill /im docker.exe /f", intern=FALSE, ignore.stdout=FALSE)
#system('shutdown -s')

Sys.time() - start


