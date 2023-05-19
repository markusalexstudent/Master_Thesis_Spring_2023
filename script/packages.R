# Packages used

library(eikonapir)                        # Refinitiv Eikon API for R
library(RSelenium)                        # Docker and Selenium
library(rvest)                            # Webscraping
library(svMisc)                           # Progress tracker
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
library(data.table)                       # Work with tables
library(textir)                           # MNIR ML Model
library(rrapply)                          # recursively apply a function to elements of a nested list based on a general condition function
library(kableExtra)                       # Create latex tables
library(multidplyr)                       # Dplyr with parallel 
library(openxlsx)                         # Save and read .xlsx
library(bizdays)                          # Business days 
library(reticulate)                       # Run Python in R
library(remotes)                          # Install remote repos (e.g., github)
library(stringr)                          # Use wrappers
library(slam)                             # Sparse Lightweight Arrays and Matrices --> convert DTM to sparse matrix
library(SentimentAnalysis)                # LM dictionary
library(felm)                             # Panel data regressions
library(wordcloud)                        # Wordcloud
library(wordcloud2)                       # Wordcloud2
library(jsonlite)                         # Create json file for fine tuning
library(openai)                           # OpenAI for fine-tuning
library(DescTools)                        # Winsorize
library(wordcloud2)                       # devtools::install_github("lchiffon/wordcloud2")

print(sessionInfo())

install.packages("NCmisc")
library(NCmisc)

list.functions.in.file("", alphabetic = TRUE)
