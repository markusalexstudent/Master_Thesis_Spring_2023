################################################################################
#                            Plots and tables                      
################################################################################
# Preamble, setting working directory
rm(list = ls())
setwd("C:/Users/marku/OneDrive/Skrivebord/MASTEROPPGAVE/Master_thesis/Data")
options(scipen = 999)
## packages
library(xml2)
library(tidyverse)          
library(tidytext)                         # Clean text
library(readxl)                           # Load excel files
library(lubridate)                        # Manipulate dates
library(dplyr)                             
library(kableExtra)
library(multidplyr)                       # Dplyr with parallel 
library(openxlsx)
library(remotes)
library(stringr)
library(slam)
library(SentimentAnalysis)
library(TTR)
library(plm)                              # Panel data
library(wordcloud)
library(wordcloud2)
library(quanteda)
library(textmineR)
library(DescTools)
library(quanteda)
library(rrtable)

#-------------------------------------------------------------------------------
# Plot event window
#-------------------------------------------------------------------------------
meta <- readRDS("fromR/metaAdj_v4.rds")      

colnames(meta)

# Calculate mean returns around an announcement:
eventWindow <- meta %>% 
  summarise(
    median_lag5 = median(abs(lag_retMkt5), na.rm = T),
    median_lag4 = median(abs(lag_retMkt), na.rm = T),
    median_lag3 = median(abs(lag_retMkt3), na.rm = T),
    median_lag2 = median(abs(lag_retMkt2), na.rm = T),
    median_lag = median(abs(lag_retMkt), na.rm = T),
    median = median(abs(retMkt), na.rm = T), 
    median_lead = median(abs(lead_retMkt), na.rm = T),
    median_lead_2 = median(abs(lead_retMkt2), na.rm = T),
    median_lead_3 = median(abs(lead_retMkt3), na.rm = T),
    median_lead_4 = median(abs(lead_retMkt4), na.rm = T),
    median_lead_5 = median(abs(lead_retMkt5), na.rm = T),
    
    ret_lag5 = mean(abs(lag_retMkt5), na.rm = T),
    ret_lag4 = mean(abs(lag_retMkt4), na.rm = T),
    ret_lag3 = mean(abs(lag_retMkt3), na.rm = T),
    ret_lag2 = mean(abs(lag_retMkt2), na.rm = T),
    ret_lag1 = mean(abs(lag_retMkt), na.rm = T),
    ret1 = mean(abs(retMkt), na.rm = T),
    lead_ret1 = mean(abs(lead_retMkt), na.rm = T),
    lead_ret2 = mean(abs(lead_retMkt2), na.rm = T),
    lead_ret3 = mean(abs(lead_retMkt3), na.rm = T),
    lead_ret4 = mean(abs(lead_retMkt4), na.rm = T),
    lead_ret5 = mean(abs(lead_retMkt5), na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  dplyr::rename(return = V1)

eventWindow <- data.frame(mean = eventWindow[12:22,1],
                          median = eventWindow[1:11,1])

eventWindow$time = c(-5,-4,-3,-2,-1,0,1,2,3,4,5)

p1 <- eventWindow %>%
  pivot_longer(1:2, names_to = "measure") %>%  
  ggplot(aes(x = time, y = value, color = measure)) + 
  geom_point(data = . %>% filter(measure == "mean"),
             size = 5, color = "blue", shape = 6) + 
  geom_line(data = . %>% filter(measure == "mean"), 
            color = "blue", linetype = "dotted", linewidth = 1, alpha = 0.7) + 
  geom_point(data = . %>% filter(measure == "median"),
             size = 5, color = "green", shape = 4) + 
  geom_line(data = . %>% filter(measure == "median"), 
            color = "green", linetype = "dashed", linewidth = 1, alpha = 0.7) + 
  geom_vline(xintercept = 0, color = "darkgray", linetype = "dotted") +
  ylab("Average/Median Absolute Excess Return") +
  xlab("Time") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) +
  labs(color = "Measure") 

p1

ggsave("tables_and_plots/plots/p1.pdf", plot = p1, width = 7, height = 5)
rm(eventWindow, p1)

#-------------------------------------------------------------------------------
# Frequency graphs
#-------------------------------------------------------------------------------
# Frequency of year:
p <- meta %>% 
  group_by(year) %>% 
  summarise(nr_articles = n()) %>% 
  filter(year < 2023) %>% 
  ggplot(aes(x=year, y = nr_articles)) + 
  geom_bar(stat="identity", fill="blue", width=.8) +
  xlab("\nYear") +
  ylab("Number of press releases\n") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) +
  scale_x_continuous(breaks = seq(min(meta$year), max(meta$year), 1), minor_breaks = NULL)

p
ggsave("tables_and_plots/plots/p10.pdf", plot = p, width = 7, height = 5)


# Frequency of tickers:
p <- meta %>% 
  group_by(ticker) %>% 
  summarise(nr_articles = n()) %>% 
  arrange(desc(nr_articles)) %>% 
  filter(nr_articles >= 100) %>% 
  mutate(ticker = fct_reorder(ticker, nr_articles)) %>%
  ggplot( aes(x=ticker, y=nr_articles)) +
  geom_bar(stat="identity", fill="blue", width=.4) +
  coord_flip() +
  xlab("") +
  ylab("\nNumber of press releases") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"))

p

ggsave("tables_and_plots/plots/p11.pdf", plot = p, width = 8, height = 12)


# Frequency of time of day: 
meta$hr <- substr(meta$time_of_day, start = 1, stop = 2)

p <- meta %>% 
  group_by(hr) %>% 
  summarise(nr_articles = n()) %>% 
  ggplot(aes(x=hr, y = nr_articles)) + 
  geom_bar(stat="identity", fill="blue", width=.9) +
  xlab("Hour of day") +
  ylab("\nTotal number of press releases") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold")) 
p

ggsave("tables_and_plots/plots/p12.pdf", plot = p, width = 7, height = 5)
colnames(meta)

# Industries:
meta$hr <- as.numeric(meta$hr)

colnames(meta)

p <- meta %>% 
  group_by(hr) %>% 
  summarise(
    nr_articles = n(), 
    ret = median(ret)) %>% 
  ggplot(aes(x=hr, y=ret, color = "blue")) +
  geom_line() + 
  guides(fill = "none")
p


#-------------------------------------------------------------------------------
# Wordcloud
#-------------------------------------------------------------------------------
# Unigrams:
toks.remove <- readRDS("robustMNIR/toksRemove.rds")
toksUni <- tokens_ngrams(as.vector(meta$text) %>% 
                        tokens(what = "word",
                               remove_numbers =T,         # Remove numbers  
                               remove_punct = T,          # Remove punctuation
                               remove_url = T,            # Remove URLs
                               remove_symbols = T) %>%
                        tokens_remove(c(stopwords(), toks.remove)), n = 1)  # 2 = bigram, 1 = unigram

dfmUni <- dfm(toksUni)
toksFreqUni <- data.frame(colSums(dfmUni))
toksFreqUni <- rownames_to_column(toksFreqUni)
toksFreqUni <- toksFreqUni[order(-toksFreqUni[,2]), ] 
toksFreqUni <- toksFreqUni[1:2000, ]
colnames(toksFreqUni) <- c("word", "n")

# Bigrams:
toks <- tokens_ngrams(as.vector(meta$text) %>% 
                           tokens(what = "word",
                                  remove_numbers =T,         # Remove numbers  
                                  remove_punct = T,          # Remove punctuation
                                  remove_url = T,            # Remove URLs
                                  remove_symbols = T) %>%
                           tokens_remove(c(stopwords(), toks.remove)), n = 2)  # 2 = bigram, 1 = unigram


dfmBi <- dfm(toks)
toksFreqBi <- data.frame(colSums(dfmBi))
toksFreqBi <- rownames_to_column(toksFreqBi)
toksFreqBi <- toksFreqBi[order(-toksFreqBi[,2]), ] 
toksFreqBi <- toksFreqBi[1:2000, ]
colnames(toksFreqBi) <- c("word", "n")
toksFreqBi$word <- gsub("_", " ", toksFreqBi$word)

toksFreq <- rbind(toksFreqUni, toksFreqBi)
toksFreq <- toksFreq[order(-toksFreq$n), ] 
rownames(toksFreq) <- toksFreq$word

wordcloud(toksFreq$word, toksFreq$n)

wc2 <- wordcloud2(toksFreq, size = 1, minSize = 0, color = "random-dark", shape = "circle", ellipticity = 0.75)
htmlwidgets::saveWidget(wc2, "fromR/wc2.html", selfcontained = F)
ifelse(webshot::is_phantomjs_installed() == "FALSE", webshot::install_phantomjs(), "OK")
webshot::webshot("fromR/wc2.html", "tables_and_plots/plots/wc2_1.pdf", vwidth = 1000, vheight = 1000, delay = 10)

lc <- letterCloud(toksFreq,"Man Vs. Machine", color = "random-dark")
htmlwidgets::saveWidget(lc, "fromR/lc3.html", selfcontained = F)
webshot::webshot("fromR/lc.html", "tables_and_plots/plots/lc1.png", vwidth = 2000, vheight = 2000, delay = 120)

# Individual words:
man <- letterCloud(toksFreq,"Man", color = "random-dark")
vs <- letterCloud(toksFreq,"vs.", color = "random-dark")
machine <- letterCloud(toksFreq,"Machine", color = "random-dark")

htmlwidgets::saveWidget(man, "tables_and_plots/plots/wordcloud/man.html", selfcontained = F)
#webshot::webshot("tables_and_plots/plots/wordcloud/man.html", "tables_and_plots/plots/wordcloud/man.png", 
#                 vwidth = 1000, vheight = 1000, delay = 10)
htmlwidgets::saveWidget(vs, "tables_and_plots/plots/wordcloud/vs.html", selfcontained = F)
#webshot::webshot("tables_and_plots/plots/wordcloud/vs.html", "tables_and_plots/plots/wordcloud/vs.png", 
#                 vwidth = 1000, vheight = 1000, delay = 10)
htmlwidgets::saveWidget(machine, "tables_and_plots/plots/wordcloud/machine.html", selfcontained = F)
#webshot::webshot("tables_and_plots/plots/wordcloud/machine.html", "tables_and_plots/plots/wordcloud/machine.png", 
#                 vwidth = 1000, vheight = 1000, delay = 10)

#-------------------------------------------------------------------------------
# Classification tables 
#-------------------------------------------------------------------------------
meta_openai <- readRDS("regressions/meta_GPT_ML_LM_regression.rds")

colnames(meta_openai)

classification <- meta_openai %>%
  summarise(across(c("lm_pos", "lm_neg", "lm_neu",
                     "mnir_uni_pos", "mnir_uni_neg", "mnir_uni_neu",
                     "mnir_bi_pos", "mnir_bi_neg", "mnir_bi_neu",
                     "curie_pos", "curie_neg", "curie_neu", 
                     "curie_pos_fine_tuned", "curie_neg_fine_tuned", "curie_neu_fine_tuned",
                     "davinci_pos", "davinci_neg", "davinci_neu",
                     "chatgpt_pos", "chatgpt_neg", "chatgpt_neu"), sum, na.rm = T)) 


classification <- matrix(classification, nrow = 3, ncol = 7)
classification <- t(classification)
colnames(classification) <- c("Positive", "Negative", "Neutral")
rownames(classification) <- c("LM",
                              "ML (uni)","ML (bi)", "Curie", "Curie (FT)", 
                              "Davinci","GPT-3.5-Turbo")

# Table 1: Classification of all approaches:
table <- classification %>% 
  kbl(format = "latex") %>% 
  kable_classic_2(html_font = "Times New Roman", full_width = F) 

table

save_kable(table, file = "tables_and_plots/tables/classification.html")

# Table 2: Classification of only ML and LM:
table <- classification[1:3, ] %>% 
  kbl(format = "latex") %>% 
  kable_classic_2(html_font = "Times New Roman", full_width = F) 

table

save_kable(table, file = "tables_and_plots/tables/classification_ML_LM.html")

# Table 3: Classification of all approaches (in %):
classification <- data.frame(classification) %>%
  rowwise() %>% 
  mutate(across(everything(), ~ round(.x / sum(c_across())*100,2))) %>% 
  ungroup() 

classification$Model <- c("LM", 
                          "ML (uni)","ML (bi)", "Curie", "Curie (FT)", 
                          "Davinci","GPT-3.5-Turbo")

classification <- column_to_rownames(classification, "Model")

table <- classification %>% 
  kbl(format = "html") %>% 
  kable_classic_2(html_font = "CMU Serif Roman", full_width = F) 

save_kable(table, file = "tables_and_plots/tables/classification_pct.html")

rm(classification)


# Classification of WSJ: 
meta_wsj <- readRDS("external_validity_wsj/meta_wsjRegression_v2")

colnames(meta_wsj)

classification <- meta_wsj %>%
  summarise(across(c("lm_pos", "lm_neg", "lm_neu",
                     "mnir_uni_pos", "mnir_uni_neg", "mnir_uni_neu",
                     "mnir_bi_pos", "mnir_bi_neg", "mnir_bi_neu",
                     "chatgpt_pos", "chatgpt_neg", "chatgpt_neu"), sum, na.rm = T)) 

classification <- matrix(classification, nrow = 3, ncol = 4)
classification <- t(classification)
colnames(classification) <- c("Positive", "Negative", "Neutral")
rownames(classification) <- c("LM",
                              "ML (uni)","ML (bi)","GPT-3.5-Turbo")

# Table 1: Classification of all approaches:
table <- classification %>% 
  kbl(format = "latex") %>% 
  kable_classic_2(html_font = "Times New Roman", full_width = F) 

table

save_kable(table, file = "tables_and_plots/tables/classification.html")

rowsum(data.frame(classification))

#-------------------------------------------------------------------------------
# Classification of human
#-------------------------------------------------------------------------------
metaHumanTesting <- readRDS("fromR/Manual_Human_Sentiment_Test/metaHumanTesting.rds")

classification <- metaHumanTesting %>%
  summarise(across(c("FinLitHu_pos", "FinLitHu_neg", "FinLitHu_neu",
                     "NonFinLitHu_pos", "NonFinLitHu_neg", "NonFinLitHu_neu",
                     "lm_pos", "lm_neg", "lm_neu",
                     "mnir_uni_pos", "mnir_uni_neg", "mnir_uni_neu",
                     "mnir_bi_pos", "mnir_bi_neg", "mnir_bi_neu",
                     "curie_pos", "curie_neg", "curie_neu", 
                     "curie_pos_fine_tuned", "curie_neg_fine_tuned", "curie_neu_fine_tuned",
                     "davinci_pos", "davinci_neg", "davinci_neu",
                     "chatgpt_pos", "chatgpt_neg", "chatgpt_neu"), sum, na.rm = T)) 

classification <- matrix(classification, nrow = 3, ncol = 9)
classification <- t(classification)
colnames(classification) <- c("Positive", "Negative", "Neutral")
rownames(classification) <- c("MSc. Fin. Student", "Preschool Teacher",
                              "LM", "ML (uni)","ML (bi)", "Curie", "Curie (FT)", 
                              "Davinci","GPT-3.5-Turbo")

# Table 1: Classification of all approaches:
table <- classification %>% 
  kbl(format = "latex") %>% 
  kable_classic_2(html_font = "Times New Roman", full_width = F) 

table

save_kable(table, file = "tables_and_plots/tables/t2.html")


#-------------------------------------------------------------------------------
# Distribution of S (sentiment scores)
#-------------------------------------------------------------------------------
hist(meta_openai$mnir_sentiBi)
hist(meta_openai$mnir_sentiUni)
hist(meta_openai$lm_senti)

colnames(meta_openai)

p2 <- meta_openai %>% 
  ggplot() + 
  geom_histogram(aes(x = mnir_sentiUni, fill = "ML (Unigrams)"), 
                 #breaks = hist(meta_openai$mnir_sentiUni, plot = F)$breaks, 
                 bins = 15,
                 alpha = .6) +
  geom_histogram(aes(x = mnir_sentiBi, fill = "ML (Bigrams)"),
                 #breaks = hist(meta_openai$mnir_sentiBi, plot = F)$breaks, 
                 bins = 15,
                 alpha = .6) +
  geom_histogram(aes(x = lm_senti, fill = "LM (Unigrams)"), 
                 #breaks = hist(meta_openai$lm_senti, plot = F)$breaks, 
                 bins = 15,
                 alpha = .6) +
  geom_vline(xintercept = 0, color = "darkgray", linetype = "longdash") +
  scale_fill_manual(values = c("blue", "red", "darkgreen")) +
  ylab("Count") +
  xlab("") +
  labs(fill = "") +
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) 

p2

ggsave("tables_and_plots/plots/p2.pdf", plot = p2, 
       width = 5, height = 5)

#-------------------------------------------------------------------------------
# Histogram of sentiment scores grid
#-------------------------------------------------------------------------------
mlUni <- meta_openai %>% 
  ggplot() +
  geom_histogram(aes(x = mnir_sentiUni), 
                 #breaks = hist(meta_openai$mnir_sentiUni, plot = F)$breaks,
                 bins = 15, 
                 fill = "blue",
                 alpha = 1) +
  geom_vline(xintercept = 0, color = "darkgray", linetype = "longdash") +
  xlab("") +
  ylab("") +
  labs(title = "ML (Unigrams)") +
  coord_cartesian(ylim = c(0, 6000)) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "top",
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))

mlBi <- meta_openai %>% 
  ggplot() +
  geom_histogram(aes(x = mnir_sentiBi), 
                 bins = 15, 
                 #breaks = hist(meta_openai$mnir_sentiBi, plot = F)$breaks,
                 fill = "red",
                 alpha = 1) +
  geom_vline(xintercept = 0, color = "darkgray", linetype = "longdash") +
  xlab("") +
  ylab("") +
  labs(title = "ML (Bigrams)") +
  coord_cartesian(ylim = c(0, 6000)) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "top",
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))

lm <- meta_openai %>% 
  ggplot() +
  geom_histogram(aes(x = lm_senti), 
                 bins = 15, 
                 #breaks = hist(meta_openai$lm_senti, plot = F)$breaks,
                 fill = "green",
                 alpha = 1) +
  geom_vline(xintercept = 0, color = "darkgray", linetype = "longdash") +
  xlab("") +
  ylab("") +
  coord_cartesian(ylim = c(0, 6000)) +
  labs(title = "LM (Unigrams)") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "top",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))
lm

#garciaBi <- meta_openai %>% 
#  ggplot() +
#  geom_histogram(aes(x = garcia_sentiBi), 
#                 breaks = hist(meta_openai$garcia_sentiBi, plot = F)$breaks,
#                 fill = "orange") +
#  geom_vline(xintercept = 0, color = "darkgray", linetype = "longdash") +
#  xlab("") +
#  ylab("") +
#  labs(title = "Garcia et al. (Bigrams)") +
#  theme_bw() +
#  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
#        legend.position = "top",
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        axis.text=element_text(size=9),
#        axis.title=element_text(size=10,face="bold"))

#garciaUni <- meta_openai %>% 
#  ggplot() +
#  geom_histogram(aes(x = garcia_sentiUni), 
#                 breaks = hist(meta_openai$garcia_sentiUni, plot = F)$breaks,
#                 fill = "purple") +
#  geom_vline(xintercept = 0, color = "darkgray", linetype = "longdash") +
#  xlab("") +
#  ylab("") +
#  labs(title = "Garcia et al. (Unigrams)") +
#  theme_bw() +
#  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
#        legend.position = "top",
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        axis.text=element_text(size=9),
#        axis.title=element_text(size=10,face="bold"))

all <- ggpubr::ggarrange(lm, mlUni, mlBi, ncol = 3, nrow = 1,
                        common.legend = TRUE, legend = "top") + 
  scale_color_manual()

all

ggsave("tables_and_plots/plots/p3.pdf", plot = all, width = 8, height = 4)

#-------------------------------------------------------------------------------
# Histogram of contemporary returns
#-------------------------------------------------------------------------------

p8 <- meta_openai %>% 
  ggplot() + 
  geom_histogram(aes(x = ret_contempMkt), 
                 bins = 100, 
  #               breaks = hist(meta_openai$ret_contempMkt, plot = F)$breaks, 
                 alpha = .6) +
  geom_vline(xintercept = 0, color = "darkgray", linetype = "longdash") +
  ylab("Count") +
  xlab("") +
  labs(fill = "") +
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) 


p8

#-------------------------------------------------------------------------------
# Time difference between each publication
#-------------------------------------------------------------------------------
meta <- readRDS("fromR/metaAdj_v4.rds") 

meta <- meta %>% 
  select(date, ticker) %>% 
  arrange(ticker, date) %>% 
  group_by(ticker) %>% 
  mutate(days_between = as.numeric(c(NA, diff(date)))) 

mean(meta$days_between, na.rm = T)

#-------------------------------------------------------------------------------
# Average return vs. classification
#-------------------------------------------------------------------------------
meta_openai <- readRDS("regressions/meta_GPT_ML_LM_regression.rds")

meta_openai <- meta_openai %>% 
  mutate(ret_contemp = Winsorize(ret_contemp, probs = c(0.01,0.99)),
         ret_contempCapm = Winsorize(ret_contempCapm, probs = c(0.01,0.99)),
         ret_contempMkt = Winsorize(ret_contempMkt, probs = c(0.01, 0.99)),
         bm = Winsorize(bm, probs = c(0.01, 0.99)),
         turnover = Winsorize(turnover, probs = c(0.01, 0.99)), 
         mcap = Winsorize(mcap, probs = c(0.01, 0.99)))


df <- meta_openai %>% 
  mutate(gpt = ifelse(chatgpt_pos == 1, "positive", 
                      ifelse(chatgpt_neg == 1, "negative", 
                             ifelse(chatgpt_neu == 1, "neutral", NA))),
         
         mlUni = ifelse(mnir_uni_pos == 1, "positive", 
                        ifelse(mnir_uni_neg == 1, "negative", 
                               ifelse(mnir_uni_neu == 1, "neutral", NA))),
         
         mlBi = ifelse(mnir_bi_pos == 1, "positive", 
                       ifelse(mnir_bi_neg == 1, "negative", 
                              ifelse(mnir_bi_neu == 1, "neutral", NA))),
         
         lm = ifelse(lm_pos == 1, "positive", 
                     ifelse(lm_neg == 1, "negative", 
                            ifelse(lm_neu == 1, "neutral", NA))),
         
  ) %>% 
  select(ret_contempMkt, gpt, mlUni, mlBi, lm) 

pdf <- data.frame(
  Positive = c(
    mean(df[df$gpt == "positive", "ret_contempMkt"]),
    mean(df[df$mlUni == "positive", "ret_contempMkt"]),
    mean(df[df$mlBi == "positive", "ret_contempMkt"]),
    mean(df[df$lm == "positive", "ret_contempMkt"])),
  Negative = c(
    mean(df[df$gpt == "negative", "ret_contempMkt"]),
    mean(df[df$mlUni == "negative", "ret_contempMkt"]),
    mean(df[df$mlBi == "negative", "ret_contempMkt"]),
    mean(df[df$lm == "negative", "ret_contempMkt"])),
  Neutral = c(
    mean(df[df$gpt == "neutral", "ret_contempMkt"]),
    mean(df[df$mlUni == "neutral", "ret_contempMkt"]),
    mean(df[df$mlBi == "neutral", "ret_contempMkt"]),
    mean(df[df$lm == "neutral", "ret_contempMkt"]))
) 

rownames(pdf) <- c("GPT-3.5-TURBO", "ML (Unigram)", "ML (Bigram)", "LM")
pdf <- rownames_to_column(pdf)

pdf <- pivot_longer(pdf, 2:4, names_to = "class", values_to = "return")

colors <- c("GPT-3.5-TURBO" = "purple", 
            "ML (Unigram)" = "green", 
            "ML (Bigram)" = "red",
            "LM" = "blue")

shapes <- c("GPT-3.5-TURBO" = 18,
            "ML (Unigram)" = 19,
            "ML (Bigram)" = 15,
            "LM" = 17)

p4 <- ggplot(data = pdf, aes(x = class, y = return, group = rowname, color = rowname, shape = rowname)) + 
  geom_line() +
  geom_point(size = 4) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  geom_hline(yintercept = c(-0.02, 0, 0.02), color = "darkgray", linetype = "dashed", size = .2) +
  labs(title = "",
       x = "Sentiment Classification", y = "Average Excess Return Around Publication", color = "", shape = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.text.x = element_text(size = 9,face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(color = "black", fill=NA)
  ) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(labels = scales::percent_format())


p4
pdf
 ggsave("tables_and_plots/plots/p4.pdf", plot = p4, width = 7, height = 5)

# Do the same as above just for GPT-models
df <- meta_openai

sum(df$curie_neu) + sum(df$curie_neg) + sum(df$curie_pos)

df <- df %>% 
  mutate(gpt_3.5 = ifelse(chatgpt_pos == 1, "positive", 
                          ifelse(chatgpt_neg == 1, "negative", 
                                 ifelse(chatgpt_neu == 1, "neutral", NA))),
         
         davinvci = ifelse(davinci_pos == 1, "positive", 
                           ifelse(davinci_neg == 1, "negative", 
                                  ifelse(davinci_neu == 1, "neutral", NA))),
         
         curieFt = ifelse(curie_pos_fine_tuned == 1, "positive", 
                          ifelse(curie_neg_fine_tuned == 1, "negative", 
                                 ifelse(curie_neu_fine_tuned == 1, "neutral", NA))),
         
         curie = ifelse(curie_pos == 1, "positive", 
                        ifelse(curie_neg == 1, "negative", 
                               ifelse(curie_neu == 1, "neutral", NA))),
         
  ) %>% 
  select(ret_contempMkt, gpt_3.5, davinvci, curieFt, curie) %>% 
  na.omit()


pdf <- data.frame(
  Positive = c(
    mean(df[df$gpt_3.5 == "positive", "ret_contempMkt"]),
    mean(df[df$davinvci == "positive", "ret_contempMkt"]),
    mean(df[df$curieFt == "positive", "ret_contempMkt"]),
    mean(df[df$curie == "positive", "ret_contempMkt"])),
  Negative = c(
    mean(df[df$gpt_3.5 == "negative", "ret_contempMkt"]),
    mean(df[df$davinvci == "negative", "ret_contempMkt"]),
    mean(df[df$curieFt == "negative", "ret_contempMkt"]),
    mean(df[df$curie == "negative", "ret_contempMkt"])),
  Neutral = c(
    mean(df[df$gpt_3.5 == "neutral", "ret_contempMkt"]),
    mean(df[df$davinvci == "neutral", "ret_contempMkt"]),
    mean(df[df$curieFt == "neutral", "ret_contempMkt"]),
    mean(df[df$curie == "neutral", "ret_contempMkt"]))
) 

rownames(pdf) <- c("GPT-3.5-TURBO", "DAVINCI", "CURIE (Fine-Tuned)", "CURIE")
pdf <- rownames_to_column(pdf)

pdf <- pivot_longer(pdf, 2:4, names_to = "class", values_to = "return")

colors <- c("GPT-3.5-TURBO" = "purple", 
            "DAVINCI" = "green", 
            "CURIE (Fine-Tuned)" = "red",
            "CURIE" = "blue")

shapes <- c("GPT-3.5-TURBO" = 18,
            "DAVINCI" = 19,
            "CURIE (Fine-Tuned)" = 15,
            "CURIE" = 17)

# Create the plot
p5 <- ggplot(data = pdf, aes(x = class, y = return, group = rowname, color = rowname, shape = rowname)) + 
  geom_line() +
  geom_point(size = 4) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  geom_hline(yintercept = c(-0.04, -0.02, 0.02, 0), color = "darkgray", linetype = "dashed", size = .2) +
  labs(title = "",
       x = "Sentiment Classification", y = "Average Excess Return Around Publication", color = "", shape = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.text.x = element_text(size = 9,face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(color = "black", fill=NA)
        ) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(labels = scales::percent_format())


p5

ggsave("tables_and_plots/plots/p5.pdf", plot = p5, width = 7, height = 5)


# Do the same for WSJ data: 
meta_wsj <- readRDS("external_validity_wsj/meta_wsjRegression_v2")
meta_wsj <- meta_wsj %>% 
  mutate(ret_contemp = Winsorize(ret_contemp, probs = c(0.01,0.99)),
         bm = Winsorize(bm, probs = c(0.01, 0.99)),
         turnover = Winsorize(turnover, probs = c(0.01, 0.99)), 
         mcap = Winsorize(mcap, probs = c(0.01, 0.99))  
  )

meta_wsj[which(meta_wsj$turnover == 0),]$turnover <- 
  meta_wsj[which(meta_wsj$turnover == 0), ]$turnover + 0.00000001

df <- meta_wsj %>% 
  mutate(gpt = ifelse(chatgpt_pos == 1, "positive", 
                      ifelse(chatgpt_neg == 1, "negative", 
                             ifelse(chatgpt_neu == 1, "neutral", NA))),
         
         mlUni = ifelse(mnir_uni_pos == 1, "positive", 
                        ifelse(mnir_uni_neg == 1, "negative", 
                               ifelse(mnir_uni_neu == 1, "neutral", NA))),
         
         mlBi = ifelse(mnir_bi_pos == 1, "positive", 
                       ifelse(mnir_bi_neg == 1, "negative", 
                              ifelse(mnir_bi_neu == 1, "neutral", NA))),
         
         lm = ifelse(lm_pos == 1, "positive", 
                     ifelse(lm_neg == 1, "negative", 
                            ifelse(lm_neu == 1, "neutral", NA))),
         
  ) %>% 
  select(ret_contemp, gpt, mlUni, mlBi, lm) %>% 
  na.omit()

df$ret_contemp <- as.numeric(df$ret_contemp)

sum(df[df$lm == "negative", ]$ret_contemp)/length(df[df$lm == "negative", ]$ret_contemp)

pdf <- data.frame(
  Positive = c(
    mean(df[df$gpt == "positive", ]$ret_contemp),
    mean(df[df$mlUni == "positive", ]$ret_contemp),
    mean(df[df$mlBi == "negative", ]$ret_contemp),
    mean(df[df$lm == "positive", ]$ret_contemp)),
  Negative = c(
    mean(df[df$gpt == "negative", ]$ret_contemp),
    mean(df[df$mlUni == "negative", ]$ret_contemp),
    mean(df[df$mlBi == "positive", ]$ret_contemp),
    mean(df[df$lm == "negative", ]$ret_contemp)),
  Neutral = c(
    mean(df[df$gpt == "neutral", ]$ret_contemp),
    mean(df[df$mlUni == "neutral", ]$ret_contemp),
    mean(df[df$mlBi == "neutral", ]$ret_contemp),
    mean(df[df$lm == "neutral", ]$ret_contemp))
) 

rownames(pdf) <- c("GPT-3.5-TURBO", "ML (Unigram)", "ML (Bigram)", "LM")
pdf <- rownames_to_column(pdf)

pdf[3, "Positive"] - pdf[3, "Neutral"]

pdf <- pivot_longer(pdf, 2:4, names_to = "class", values_to = "return")


colors <- c("GPT-3.5-TURBO" = "purple", 
            "ML (Unigram)" = "green", 
            "ML (Bigram)" = "red",
            "LM" = "blue")
shapes <- c("GPT-3.5-TURBO" = 18,
            "ML (Unigram)" = 19,
            "ML (Bigram)" = 15,
            "LM" = 17)

p9 <- ggplot(data = pdf, aes(x = class, y = return, group = rowname, color = rowname, shape = rowname)) + 
  geom_line() +
  geom_point(size = 4) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  geom_hline(yintercept = c(-0.005, 0, 0.005), color = "darkgray", linetype = "dashed", size = .2) +
  labs(title = "",
       x = "Sentiment Classification", y = "Average Excess Return Around Publication", color = "", shape = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.text.x = element_text(size = 9,face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(color = "black", fill=NA)
  ) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(labels = scales::percent_format())


p9


ggsave("tables_and_plots/plots/p9.pdf", plot = p9, width = 7, height = 5)

#------------------------------------------------------------------------------

reg <- readRDS("Regressions/reg_lists/reg_wsj.rds")


pdf <- data.frame(
  Positive = c(),
  Negative = c(),
  Neutral = c()
)




#-------------------------------------------------------------------------------
# Tables of top 20 ML Bi/Unigrams
#-------------------------------------------------------------------------------
rm(list = ls())
mlUniPos <- readRDS("robustMNIR/mlUniPos.rds")
mlUniNeg <- readRDS("robustMNIR/mlUniNeg.rds")
mlBiPos <- readRDS("robustMNIR/mlBiPos.rds")
mlBiNeg <- readRDS("robustMNIR/mlBiNeg.rds")

mlUniPos <- mlUniPos[order(mlUniPos$positive, decreasing = T), ]
mlUniNeg <- mlUniNeg[order(mlUniNeg$negative, decreasing = T), ]
mlBiPos <- mlBiPos[order(mlBiPos$positive, decreasing = T), ]
mlBiNeg <- mlBiNeg[order(mlBiNeg$negative, decreasing = T), ]
mlBiPos$word <- gsub("_", " ", mlBiPos$word)
mlBiNeg$word <- gsub("_", " ", mlBiNeg$word)

mlUniPos[mlUniPos$word == "amount", ]

mlBiPos$word[grepl("negative", mlBiPos$word, ignore.case = TRUE)]


top20 <- data.frame(mlUniNeg = head(mlUniNeg$word, 20), 
                    mlBiNeg = head(mlBiNeg$word, 20),
                    mlUniPos = head(mlUniPos$word, 20),
                    mlBiPos = head(mlBiPos$word, 20))

top20$mlUniNeg <- ifelse(top20$mlUniNeg == "tuesday", 
                         mlUniNeg[mlUniNeg$word == "split", ]$word, 
                         top20$mlUniNeg)

top20$mlUniNeg <- ifelse(top20$mlUniNeg == "cad", 
                         mlUniNeg[mlUniNeg$word == "abandoned", ]$word, 
                         top20$mlUniNeg)

top20$mlUniNeg <- ifelse(top20$mlUniNeg == "srs", 
                         mlUniNeg[mlUniNeg$word == "investigation", ]$word, 
                         top20$mlUniNeg)

top20$mlUniNeg <- ifelse(top20$mlUniNeg == "maximum", 
                         mlUniNeg[mlUniNeg$word == "bankruptcy", ]$word, 
                         top20$mlUniNeg)

top20$mlUniNeg <- ifelse(top20$mlUniNeg == "informed", 
                         mlUniNeg[mlUniNeg$word == "delay", ]$word, 
                         top20$mlUniNeg)

top20$mlUniNeg <- ifelse(top20$mlUniNeg == "via", 
                         mlUniNeg[mlUniNeg$word == "strike", ]$word, 
                         top20$mlUniNeg)

top20$mlUniNeg <- ifelse(top20$mlUniNeg == "fourth", 
                         mlUniNeg[mlUniNeg$word == "cancelled", ]$word, 
                         top20$mlUniNeg)

top20$mlUniPos <- ifelse(top20$mlUniPos == "antimicrobial", 
                         mlUniPos[mlUniPos$word == "synergies", ]$word, 
                         top20$mlUniPos)

top20$mlUniPos <- ifelse(top20$mlUniPos == "wound", 
                         mlUniPos[mlUniPos$word == "potentially", ]$word, 
                         top20$mlUniPos)

top20$mlUniPos <- ifelse(top20$mlUniPos == "manufacturers", 
                         mlUniPos[mlUniPos$word == "disruptive", ]$word, 
                         top20$mlUniPos)

top20$mlUniPos <- ifelse(top20$mlUniPos == "firm", 
                         mlUniPos[mlUniPos$word == "grow", ]$word, 
                         top20$mlUniPos)

top20$mlUniPos <- ifelse(top20$mlUniPos == "industries", 
                         mlUniPos[mlUniPos$word == "loi", ]$word, 
                         top20$mlUniPos)

top20$mlUniPos <- ifelse(top20$mlUniPos == "zwipe", 
                         mlUniPos[mlUniPos$word == "proven", ]$word, 
                         top20$mlUniPos)

top20$mlUniPos <- ifelse(top20$mlUniPos == "regimen", 
                         mlUniPos[mlUniPos$word == "efficient", ]$word, 
                         top20$mlUniPos)

top20$mlBiNeg <- ifelse(top20$mlBiNeg == "beginning one", 
                        mlBiNeg[mlBiNeg$word == "unlawful announcement",]$word,
                        top20$mlBiNeg)
                      
top20$mlBiNeg <- ifelse(top20$mlBiNeg == "registered held", 
                        mlBiNeg[mlBiNeg$word == "offering unlawful",]$word,
                        top20$mlBiNeg)

top20$mlBiNeg <- ifelse(top20$mlBiNeg == "notwithstanding foregoing", 
                        mlBiNeg[mlBiNeg$word == "reverse split",]$word,
                        top20$mlBiNeg)

top20$mlBiPos <- ifelse(top20$mlBiPos == "gaming industry", 
                        mlBiPos[mlBiPos$word == "continues grow",]$word,
                        top20$mlBiPos)

top20$mlBiPos <- ifelse(top20$mlBiPos == "igaming industry", 
                        mlBiPos[mlBiPos$word == "quality content",]$word,
                        top20$mlBiPos)

top20$mlBiPos <- ifelse(top20$mlBiPos == "b??rs company", 
                        mlBiPos[mlBiPos$word == "machine learning",]$word,
                        top20$mlBiPos)

top20$mlBiPos <- ifelse(top20$mlBiPos == "gaming innovation", 
                        mlBiPos[mlBiPos$word == "charter contract",]$word,
                        top20$mlBiPos)

top20$mlBiPos <- ifelse(top20$mlBiPos == "also refer", 
                        mlBiPos[mlBiPos$word == "major milestone",]$word,
                        top20$mlBiPos)


colnames(top20) <- c("Unigram", "Bigram", "Unigram", "Bigram")
top20 

t1 <- top20 %>% 
  kbl(format = "latex", escape = F) %>%
  kable_classic_2(html_font = "Times New Roman", full_width = F) %>% 
  add_header_above(c("Negative" = 2,"Positive" = 2), 
                   color = c("red", "blue"), bold = T)

t1

save_kable(t1, file = "tables_and_plots/tables/t1.html")

#-------------------------------------------------------------------------------
# Sensitivity analysis of threshold values (th) with Adj. R2
#-------------------------------------------------------------------------------
rm(list = ls())
meta <- readRDS("robustMNIR/meta_ML_LM_complete.rds")

meta <- meta %>% 
  mutate(
    ret_contempMkt = Winsorize(ret_contempMkt, probs = c(0.01,0.99)),
    bm = Winsorize(bm, probs = c(0.01, 0.99)),
    turnover = Winsorize(turnover, probs = c(0.01, 0.99)), 
    mcap = Winsorize(mcap, probs = c(0.01, 0.99))
  )

th <- seq(0, 0.5, by = 0.05)
mat <- matrix(NA, nrow = length(th), ncol = 4)
j <- 1

for(i in th) {
  meta <- meta %>% 
    mutate(
      mnir_bi_neu = ifelse(mnir_sentiBi > -i & mnir_sentiBi < i, 1, 0),
      mnir_bi_pos = ifelse(mnir_sentiBi >= i, 1, 0),
      mnir_bi_neg = ifelse(mnir_sentiBi <= -i, 1, 0),
      
      mnir_uni_neu = ifelse(mnir_sentiUni > -i & mnir_sentiUni < i, 1, 0),
      mnir_uni_pos = ifelse(mnir_sentiUni >= i, 1, 0),
      mnir_uni_neg = ifelse(mnir_sentiUni <= -i, 1, 0),
      
      lm_neu = ifelse(lm_senti > -i & lm_senti < i, 1, 0),
      lm_pos = ifelse(lm_senti >= i, 1, 0),
      lm_neg = ifelse(lm_senti <= -i, 1, 0)
    )
  
  # Add pseudo-count, as there are some volumes that are zero. (log of zero dont work)
  meta[which(meta$turnover == 0),]$turnover <- 
    meta[which(meta$turnover == 0), ]$turnover + 0.00000001
  
  mlBi <- lfe::felm(
    ret_contempMkt ~ mnir_bi_pos + mnir_bi_neg + log(words) + log(bm) + 
      log(turnover) + log(mcap)  
    | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
    data = meta %>% filter(group == "holdout"), psdef=FALSE)
  
  mlUni <- lfe::felm(
    ret_contempMkt ~ mnir_uni_pos + mnir_uni_neg + log(words) + log(bm) + 
      log(turnover) + log(mcap)  
    | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
    data = meta %>% filter(group == "holdout"), psdef=FALSE)
  
  lm <- lfe::felm(
    ret_contempMkt ~ lm_pos + lm_neg + log(words) + log(bm) + 
      log(turnover) + log(mcap)  
    | trbc_industry + qq_yyyy | 0 | trbc_industry + qq_yyyy,
    data = meta %>% filter(group == "holdout"), psdef=FALSE)
  
  mat[j, 4] <- summary(mlBi)$adj.r.squared
  mat[j, 3] <- summary(mlUni)$adj.r.squared
  mat[j, 2] <- summary(lm)$adj.r.squared
  mat[j, 1] <- i
  j <- j + 1
}

df <- data.frame(mat)
colnames(df) <- c("Threshold", "LM", "ML (Unigrams)", "ML (Bigrams)")
df <- pivot_longer(df, 2:4, names_to = "model", values_to = "Adj. R2")

colors <- c("LM" = "green", 
            "ML (Unigrams)" = "blue", 
            "ML (Bigrams)" = "red")

shapes <- c("LM" = 0,
            "ML (Unigrams)" = 3,
            "ML (Bigrams)" = 6)

p6 <- df %>% 
  ggplot(aes(x = Threshold, y = `Adj. R2`, group = model, color = model, shape = model)) + 
  geom_line() +
  geom_point(size = 4) +
  geom_vline(xintercept = max(th)/2, linetype = "dashed", color = "darkgray") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  labs(title = "",
       x = "Threshold", y = "Adjusted R2", color = "", shape = "") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.text.x = element_text(size = 9,face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(color = "black", fill=NA)
  ) +
  scale_y_continuous(labels = scales::percent_format())

p6

ggsave("tables_and_plots/plots/p6.pdf", plot = p6, width = 7, height = 5)

#-------------------------------------------------------------------------------
# Sensitivity with different thresholds
# Classification (neutral, negative, positive on Y-axis)
#-------------------------------------------------------------------------------
# Load data:
meta_openai <- readRDS("regressions/meta_GPT_ML_LM_regression.rds")

seq <- seq(0, 0.5, by = 0.05)

colnames(meta_openai)

class <- data.frame()

for(i in seq){
  result <- meta_openai %>% 
    
    mutate(neuLM = ifelse(lm_senti > -i & lm_senti < i, 1, 0),
           posLM = ifelse(lm_senti >= i, 1, 0),
           negLM = ifelse(lm_senti <= -i, 1, 0),
           
           neuUni = ifelse(mnir_sentiUni > -i & mnir_sentiUni < i, 1, 0),
           posUni = ifelse(mnir_sentiUni >= i, 1, 0),
           negUni = ifelse(mnir_sentiUni <= -i, 1, 0),
           
           neuBi = ifelse(mnir_sentiBi > -i & mnir_sentiBi < i, 1, 0),
           posBi = ifelse(mnir_sentiBi >= i, 1, 0),
           negBi = ifelse(mnir_sentiBi <= -i, 1, 0),
    ) %>% 
    
    summarise(posLM = sum(posLM, na.rm = T),
              negLM = sum(negLM, na.rm = T),
              neuLM = sum(neuLM, na.rm = T),
              
              posMLUni = sum(posUni, na.rm = T),
              negMLUni = sum(negUni, na.rm = T),
              neuMLUni = sum(neuUni, na.rm = T),
              
              posMLBi = sum(posBi, na.rm = T),
              negMLBi = sum(negBi, na.rm = T),
              neuMLBi = sum(neuBi, na.rm = T))
  
  result$th <- i
  
  class <- rbind(class, result)
  
}

class <- pivot_longer(class, 1:9, names_to = "classification")
class$sentiment <- substr(class$classification, 1, 3)
class$method <- substr(class$classification, 4, 8)
class$method <- gsub("MLUni", "ML (Unigrams)", class$method)
class$method <- gsub("MLBi", "ML (Bigrams)", class$method)

colors <- c("LM" = "green", 
            "ML (Unigrams)" = "blue", 
            "ML (Bigrams)" = "red")

shapes <- c("LM" = 18,
            "ML (Unigrams)" = 19,
            "ML (Bigrams)" = 15)

pos <- class %>%
  filter(sentiment == "pos") %>% 
  ggplot(aes(x = th, y = value, color = method)) + 
  geom_line() +
  geom_vline(xintercept = max(class$th)/2, color = "darkgray", linetype = "longdash") + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  ggtitle("Positive") +
  labs(color = "") +
  coord_cartesian(ylim = c(0, 12000)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        legend.text = element_text(size = 14),
       # legend.text = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))  

pos

neg <- class %>%
  filter(sentiment == "neg") %>% 
  ggplot(aes(x = th, y = value, color = method)) + 
  geom_line() +
  geom_vline(xintercept = max(class$th)/2, color = "darkgray", linetype = "longdash") + 
  ylab("") +
  xlab("Threshold") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  ggtitle("Negative") +
  coord_cartesian(ylim = c(0, 12000)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))  


neu <- class %>%
  filter(sentiment == "neu") %>% 
  ggplot(aes(x = th, y = value, color = method)) + 
  geom_line() +
  geom_vline(xintercept = max(class$th)/2, color = "darkgray", linetype = "longdash") + 
  ylab("") +
  xlab("") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  ggtitle("Neutral") +
  labs(color = "") +
  coord_cartesian(ylim = c(0, 12000)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))  


p7 <- ggpubr::ggarrange(pos, neg, neu, ncol = 3, nrow = 1, 
                        common.legend = TRUE, legend = "top") + 
  scale_color_manual()

p7

ggsave("tables_and_plots/plots/p7.pdf", plot = p7, 
      width = 10, height = 5)

#-------------------------------------------------------------------------------
# Mean returns around publication of WSJ
#-------------------------------------------------------------------------------
meta_wsj <- readRDS("external_validity_wsj/meta_wsjRegression_v2")

mean(abs(meta_wsj$ret_contemp))
mean(abs(meta_wsj$ret))
mean(abs(meta_wsj$lag_ret))
mean(abs(meta_wsj$lead_ret))

#-------------------------------------------------------------------------------
# Exploring the top LM terms:
#-------------------------------------------------------------------------------
rm(list = ls())
mlUniPos <- readRDS("robustMNIR/mlUniPos.rds")
mlUniNeg <- readRDS("robustMNIR/mlUniNeg.rds")
mlBiPos <- readRDS("robustMNIR/mlBiPos.rds")
mlBiNeg <- readRDS("robustMNIR/mlBiNeg.rds")
lmPos <- DictionaryLM$positive
lmNeg <- DictionaryLM$negative

dtm <- readRDS("robustMNIR/dtm_unigram.rds")
dtmBi <- readRDS("robustMNIR/dtm_bigram.rds")
freqLM <- sort(col_sums(dtm[,colnames(dtm) %in% c(lmPos, lmNeg)]), decreasing = TRUE)
freqMLUni <- sort(col_sums(dtm[,colnames(dtm) %in% c(mlUniPos$word, mlUniNeg$word)]), decreasing = TRUE)
freqMLBi <- sort(col_sums(dtmBi[,colnames(dtmBi) %in% c(mlBiPos$word, mlBiNeg$word)]), decreasing = TRUE)

#write.xlsx(rownames_to_column(data.frame(freqLM)), file = "tables_and_plots/freqLM.xlsx")
#write.xlsx(rownames_to_column(data.frame(freqMLUni)), file = "tables_and_plots/freqMLUni.xlsx")
#write.xlsx(rownames_to_column(data.frame(freqMLBi)), file = "tables_and_plots/freqMLBi.xlsx")

# Check dimensions: 
dim(dtm)
dim(dtmBi)

#-------------------------------------------------------------------------------
# Frequency vs. IDF
#-------------------------------------------------------------------------------
dtm <- sparseMatrix(i = dtm$i, 
                    j = dtm$j, 
                    x = dtm$v, 
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))

idf <- TermDocFreq(dtm)
idf <- idf[order(idf$term_freq, decreasing = T), ]

idf <- idf %>% 
  mutate(share = term_freq/sum(term_freq),
         cumShare = cumsum(share))

# Create a sequence:
idf_rank <- seq(from = 0, to = 10, by = 0.5)

idf <- idf %>%
  mutate(idf = round(idf)) %>% 
  filter(idf %in% idf_rank) %>% 
  distinct() %>% 
  group_by(idf) %>% 
  summarise(cumShare = max(cumShare))

idf$ngram <- "Unigrams"

plot(idf$idf, idf$cumShare)

dtmBi <- sparseMatrix(i = dtmBi$i, 
                    j = dtmBi$j, 
                    x = dtmBi$v, 
                    dims = c(dtmBi$nrow, dtmBi$ncol),
                    dimnames = list(rownames(dtmBi), colnames(dtmBi)))

idfBi <- TermDocFreq(dtmBi)

idfBi <- idfBi[order(idfBi$term_freq, decreasing = T), ]
idfBi <- idfBi %>% 
  mutate(share = term_freq/sum(term_freq),
         cumShare = cumsum(share))

idfBi <- idfBi %>%
  mutate(idf = round(idf)) %>% 
  filter(idf %in% idf_rank) %>% 
  distinct() %>% 
  group_by(idf) %>% 
  summarise(cumShare = max(cumShare))

idfBi$ngram <- "Bigrams"

idf <- rbind(idf, idfBi)


colors <- c("Unigrams" = "blue", 
            "Bigrams" = "red")

shapes <- c("Unigrams" = 3,
            "Bigrams" = 6)

p8 <- idf %>%
  ggplot(aes(x = idf, y = cumShare, color = ngram, shape = ngram)) + 
  geom_point(size = 4) + 
  ylab("Share of total corpus") +
  xlab("IDF Score") +
  labs(color = NULL, shape = NULL, guide = "none") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) +
  scale_y_continuous(labels = scales::percent_format())  
p8  


ggsave("tables_and_plots/plots/p8.pdf", plot = p8, 
       width = 7, height = 5)

#-------------------------------------------------------------------------------
# Frequency vs. IDF v2
#-------------------------------------------------------------------------------
dtm <- readRDS("robustMNIR/dtm_unigram.rds")
dtm <- sparseMatrix(i = dtm$i, 
                    j = dtm$j, 
                    x = dtm$v, 
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))

idf <- TermDocFreq(dtm)
idf <- idf[order(idf$idf, decreasing = F), ]

idf$share <- (idf$doc_freq / (sum(idf$doc_freq)/nrow(idf))) / nrow(idf) 
sum(idf$share)

idf$cumShare <- cumsum(idf$share)

# Create a sequence:
idf_rank <- seq(from = 0, to = 10, by = 0.5)

idf <- idf %>%
  mutate(idf = round(idf)) %>% 
  filter(idf %in% idf_rank) %>% 
  distinct() %>% 
  group_by(idf) %>% 
  summarise(cumShare = max(cumShare))

idf$ngram <- "Unigrams"

plot(idf$idf, idf$cumShare)

dtmBi <- readRDS("robustMNIR/dtm_bigram.rds")
dtmBi <- sparseMatrix(i = dtmBi$i, 
                      j = dtmBi$j, 
                      x = dtmBi$v, 
                      dims = c(dtmBi$nrow, dtmBi$ncol),
                      dimnames = list(rownames(dtmBi), colnames(dtmBi)))

idfBi <- TermDocFreq(dtmBi)
idfBi <- idfBi[order(idfBi$idf, decreasing = F), ]

idfBi$share <- (idfBi$doc_freq / (sum(idfBi$doc_freq)/nrow(idfBi))) / nrow(idfBi) 
sum(idfBi$share)

idfBi$cumShare <- cumsum(idfBi$share)

idfBi <- idfBi %>%
  mutate(idf = round(idf)) %>% 
  filter(idf %in% idf_rank) %>% 
  distinct() %>% 
  group_by(idf) %>% 
  summarise(cumShare = max(cumShare))

idfBi$ngram <- "Bigrams"

plot(idfBi$idf, idfBi$cumShare)

idf <- rbind(idf, idfBi)


colors <- c("Unigrams" = "blue", 
            "Bigrams" = "red")

shapes <- c("Unigrams" = 3,
            "Bigrams" = 6)

p8 <- idf %>%
  ggplot(aes(x = idf, y = cumShare, color = ngram, shape = ngram)) + 
  geom_point(size = 4) + 
  ylab("Share of total corpus") +
  xlab("IDF Score") +
  labs(color = NULL, shape = NULL, guide = "none") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) +
  scale_y_continuous(labels = scales::percent_format())  
p8  


ggsave("tables_and_plots/plots/p8.pdf", plot = p8, 
       width = 7, height = 5)

#-------------------------------------------------------------------------------
# Doc freq. vs. IDF
#-------------------------------------------------------------------------------
dtm <- readRDS("robustMNIR/dtm_unigram.rds")
dtm <- sparseMatrix(i = dtm$i, 
                    j = dtm$j, 
                    x = dtm$v, 
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))

idf <- TermDocFreq(dtm)
idf <- idf[order(idf$idf, decreasing = F), ]
idf$doc_freq <- idf$doc_freq/nrow(dtm)

# Create a sequence:
idf_rank <- seq(from = 0, to = 10, by = 0.5)

idf <- idf %>%
  mutate(idf = round(idf)) %>% 
  filter(idf %in% idf_rank) %>% 
  distinct() %>% 
  group_by(idf) %>% 
  summarise(doc_freq = max(doc_freq))

idf$ngram = "Unigrams"

dtmBi <- readRDS("robustMNIR/dtm_bigram.rds")
dtmBi <- sparseMatrix(i = dtmBi$i, 
                      j = dtmBi$j, 
                      x = dtmBi$v, 
                      dims = c(dtmBi$nrow, dtmBi$ncol),
                      dimnames = list(rownames(dtmBi), colnames(dtmBi)))

idfBi <- TermDocFreq(dtmBi)
idfBi <- idfBi[order(idfBi$idf, decreasing = F), ]
idfBi$doc_freq <- idfBi$doc_freq/nrow(dtmBi)

idfBi <- idfBi %>%
  mutate(idf = round(idf)) %>% 
  filter(idf %in% idf_rank) %>% 
  distinct() %>% 
  group_by(idf) %>% 
  summarise(doc_freq = max(doc_freq))

idfBi$ngram <- "Bigrams"

idf <- rbind(idf, idfBi)

colors <- c("Unigrams" = "blue", 
            "Bigrams" = "red")

shapes <- c("Unigrams" = 3,
            "Bigrams" = 6)

p8 <- idf %>%
  ggplot(aes(x = idf, y = doc_freq, color = ngram, shape = ngram)) + 
 # scale_y_log10() +
  geom_point(size = 4) + 
  ylab("Document Frequency (in % of total documents)") +
  xlab("IDF Score") +
  labs(color = NULL, shape = NULL, guide = "none") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top",
        legend.text = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) +
  scale_y_continuous(labels = scales::percent_format()) 
p8  


#-------------------------------------------------------------------------------
# Share of total corpus vs. IDF
#-------------------------------------------------------------------------------
dtm <- readRDS("robustMNIR/dtm_unigram.rds")
dtm <- sparseMatrix(i = dtm$i, 
                    j = dtm$j, 
                    x = dtm$v, 
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))

idf <- TermDocFreq(dtm)
idf <- idf[order(idf$idf, decreasing = F), ]

seq <- c(seq(from = 1, to = 8, by = 0.5))
mat <- matrix(NA, nrow = length(seq), ncol = 3)
mat[,1] <- seq
colnames(mat) <- c("idf", "Unigrams", "Bigrams")

d <- 1
for(i in seq){

  idfFilter <- idf[idf$idf <= i, ]
  dtmIdf <- dtm[,idfFilter$term, drop = FALSE]
  
  mat[d,2] <- sum(row_sums(dtmIdf) != 0)/ nrow(dtm)
  d <- d + 1
}

dtm <- readRDS("robustMNIR/dtm_bigram.rds")
dtm <- sparseMatrix(i = dtm$i, 
                    j = dtm$j, 
                    x = dtm$v, 
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))

idf <- TermDocFreq(dtm)
idf <- idf[order(idf$idf, decreasing = F), ]

d <- 1
for(i in seq){
  
  idfFilter <- idf[idf$idf <= i, ]
  dtmIdf <- dtm[,idfFilter$term, drop = FALSE]
  
  mat[d,3] <- sum(row_sums(dtmIdf) != 0)/nrow(dtm)
  d <- d + 1
}

df <- data.frame(mat)
df <- pivot_longer(df, 2:3, values_to = "count", names_to = "ngram")
df$idf <- as.numeric(df$idf)

docFreq <- df

colors <- c("Unigrams" = "blue", 
            "Bigrams" = "red")

shapes <- c("Unigrams" = 3,
            "Bigrams" = 6)

p8 <- df %>%
  ggplot(aes(x = idf, y = count, color = ngram, shape = ngram)) + 
#  scale_x_log10() +
  geom_point(size = 4) + 
  ylab("Share of total corpus") +
  scale_x_continuous(breaks = unique(df$idf)) +
  xlab("IDF Score") +
  labs(color = NULL, shape = NULL, guide = "none") +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  geom_vline(xintercept = 7, linetype = "dashed", color = "darkgrey") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top",
        #legend.text = element_text(face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) +
  scale_y_continuous(labels = scales::percent_format()) 
p8  

ggsave("tables_and_plots/plots/p8.pdf", plot = p8, width = 7, height = 5)

#-------------------------------------------------------------------------------
# Term freq. vs. IDF
#-------------------------------------------------------------------------------
# Unigrams:
dtm <- readRDS("robustMNIR/dtm_unigram.rds")
dtm <- sparseMatrix(i = dtm$i, 
                    j = dtm$j, 
                    x = dtm$v, 
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))

idf <- TermDocFreq(dtm)
idf <- idf[order(idf$term_freq, decreasing = T), ]
idf$rank  <- 1:nrow(idf)

seq <- 4^(0:9)
mat <- matrix(NA, nrow = length(seq), ncol = 3)
mat[,1] <- seq
colnames(mat) <- c("rank", "Unigrams", "Bigrams")

d <- 1
for(i in seq){
  
  idfFilter <- idf[idf$rank <= i, ]
  dtmIdf <- dtm[,idfFilter$term, drop = FALSE]
  
  mat[d,2] <- sum(row_sums(dtmIdf) != 0)
  d <- d + 1
}

mat

# Bigrams:
dtm <- readRDS("robustMNIR/dtm_bigram.rds")
dtm <- sparseMatrix(i = dtm$i, 
                    j = dtm$j, 
                    x = dtm$v, 
                    dims = c(dtm$nrow, dtm$ncol),
                    dimnames = list(rownames(dtm), colnames(dtm)))

idf <- TermDocFreq(dtm)
idf <- idf[order(idf$term_freq, decreasing = T), ]
idf$rank  <- 1:nrow(idf)

d <- 1
for(i in seq){
  
  idfFilter <- idf[idf$rank <= i, ]
  dtmIdf <- dtm[,idfFilter$term, drop = FALSE]
  
  mat[d,3] <- sum(row_sums(dtmIdf) != 0)
  d <- d + 1
}

mat

df <- data.frame(mat)
df <- pivot_longer(df, 2:3, values_to = "count", names_to = "ngram")
df$rank <- as.numeric(df$rank)

colors <- c("Unigrams" = "blue", 
            "Bigrams" = "red")

shapes <- c("Unigrams" = 3,
            "Bigrams" = 6)

p12 <- df %>%
  ggplot(aes(x = rank, y = count, color = ngram, shape = ngram)) + 
  scale_x_log10() +
  geom_point(size = 4) 
p12




df[df$idf == 7 & df$ngram == "Unigrams", ]$count
df[df$idf == 7 & df$ngram == "Bigrams", ]$count

#ggsave("tables_and_plots/plots/p8.pdf", plot = p8, width = 7, height = 5)

#-------------------------------------------------------------------------------
# Term freq vs. IDF freq. 
#-------------------------------------------------------------------------------
idf <- cbind(docFreq, termFreq = termFreq$count)
idf <- tibble(idf)
idf <- idf %>% 
  filter(ngram == "Unigrams")

colnames(idf) <- c("idf", "ngram", "docFreq", "termFreq")

library(plotly)

plot_ly(data = idf, x = ~idf, y = ~docFreq, z = ~termFreq, colors = ~ngram)


#-------------------------------------------------------------------------------
# Search for words
#-------------------------------------------------------------------------------
meta <- readRDS("fromR/metaAdj_v4.rds")
search <- meta[grepl("profit", meta$text, ignore.case = TRUE), ]


search <- DictionaryLM[grepl("investigation", DictionaryLM, ignore.case = TRUE)]
DictionaryLM$negative[grepl("delay", DictionaryLM$negative, ignore.case = TRUE)]
DictionaryLM$positive[grepl("loi", DictionaryLM$positive, ignore.case = TRUE)]


#-------------------------------------------------------------------------------
# Create latex table for stopwords
#-------------------------------------------------------------------------------
knitr::kable(data.frame(stopWords = stopwords()), format = "latex", booktabs = TRUE, col.names = NULL)

toks.remove <- c("asa", "disclosure", "publication", "requirements", 
                 "act", "distribution",
                 "aalborg", "aak", "aarhus", "aaa", "aa", "a",
                 "quarter", "first quarter", "second quarter", "third quarter", 
                 "fourth quarter", "aadhaar", "aacr", "aardal",
                 "ab publ", "ab", "http", "acc", 
                 "vphl", "aasen", 
                 "jurisdiction","ceo", "managing", "director", 
                 "key information", "co", "ltd", "vice", "president", "aaog",
                 "isin", "link", "webcast", "alia")
data.frame(toks.remove)





