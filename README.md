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
