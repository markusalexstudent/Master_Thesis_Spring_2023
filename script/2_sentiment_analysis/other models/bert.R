################################################################################
#                                 BERT 
################################################################################
rm(list = ls())
setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/GITHUB REPOSITORY/Master_Thesis_Spring_2023/Data/")

library(tidyverse)
library(reticulate)
library(LiblineaR)
library(tidymodels)
library(rsample)

# Python needed:
reticulate::py_install('transformers', pip = TRUE)
reticulate::py_install('tensorflow')
reticulate::py_install('builtins')

# Import Python packages:
transformer = reticulate::import('transformers')
tf = reticulate::import('tensorflow')
builtins <- reticulate::import_builtins() #built in python methods

# Load data: 
meta <- readRDS("fromR/metaAdj_v4.rds")
meta <- meta[100:400,]

metaTraining <- meta %>% 
  filter(group == "training")

metaTest <- meta %>% 
  filter(group == "holdout")

train_encodings = tokenize(metaTraining$text, truncation=TRUE, padding=TRUE,max_length=250L)



