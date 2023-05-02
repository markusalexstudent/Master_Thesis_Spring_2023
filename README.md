# Man vs. Machine | Master Thesis Spring 2023
## An applied study comparing a man-made lexicon, a machine learned lexicon, and OpenAI's GPT for sentiment analysis.
Master thesis in Financial Economics at NHH spring 2023. All files related to the project lies here. 
 

# Info
General Info: 
- The R-script is split into parts, and should be run chronologically. 
- Data is in the "data" folder.
  - Plots and tables are in "tables_and_plots".
  - ML lexicons are in "dictionary".
  - Robust MNIR outputs (w/ idf score) are in "robustMNIR".
  - An csv-file with all scraped press releases can be found in the data folder.
- R script is in the "script" folder.
- You do not need to run the script from "scratch", i.e. web scrape and run ML/OpenAI Models (these are very time consuming). Just download the data, set working directionary in the script and then run the script. 

What you need to run the complete script (besides packages in the script): 
- OpenAI API key. Create an account on https://openai.com/
- Refinitiv Eikon API key. (If you are a student at NHH you can ask the library for a license --> https://nhh.libguides.com/az.php)
- Docker (needed for web scraping NewsWeb) https://www.docker.com/

## R Libraries:
```r 
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

```
## Refinitiv Eikon API for R:
```r
install_github("ahmedmohamedali/eikonapir") # Installation of Eikon API for R

# Log on Refinitiv Desktop
# Get API key --> APPKEY --> Register new app (API: Eikon Data API)
# Find data keys --> Data Item Browser

```

