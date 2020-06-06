##### February 2020
##### Business Insight Report Tesla Feb 2020 - Text Analytics
##### by David Rubio - MSc Business Analytics
##### Hult International Business School

library(textreadr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(stringr)
library(ggplot2)
library(textdata)
library(reshape2)
library(wordcloud)
library(igraph)
library(ggraph)

tesla_bat <- read_document(file="/Users/XXXXX") #DEFINE YOUR OWN PATH
tesla_jail <- read_document(file="/Users/XXXXX") #DEFINE YOUR OWN PATH
tesla_solar <- read_document(file="/Users/XXXXX") #DEFINE YOUR OWN PATH
tesla_accident <- read_document(file="/Users/XXXXX") #DEFINE YOUR OWN PATH
tesla_loan <- read_document(file="/Users/XXXXX") #DEFINE YOUR OWN PATH
file_combo <- c(tesla_bat, tesla_jail, tesla_solar, tesla_accident, tesla_loan)

#####converting vector into a data frame
tesla_df <- tibble(line=1:54, text=file_combo)

data(stop_words)

#####creating a custom dictionary of 'stop words'
custom_stop <- data.frame(
  word=c("tesla","ntsb","sadow", "tesla's", "car", "cars"),
  lexicon=rep("CUSTOM", each=6)
)

#####tokenization 
tesla_token <- tesla_df %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE)

#####removal of stop words
tesla_clean <- tesla_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

#####removal of custom stop words
tesla_tidy <- tesla_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop) %>%
  count(word, sort=TRUE) 

#####token frequency histogram
tesla_hist <- tesla_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop) %>%
  count(word, sort=TRUE) %>%
  top_n(20) %>%
  ggplot(aes(word, n, fill=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(tesla_hist)

#####Histogram shows the news are specially related to the 'autopilot' and 'software' terms. But why?


#####Creation of word cloud
tesla_wordcloud <- tesla_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop) %>%
  inner_join(get_sentiments("nrc")) %>% ##Getting nrc sentiments
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=1000,
                   scale = c(0.5, 0.5),
                   fixed.asp = TRUE,
                   title.size = 1
  )

#The sentiment word cloud reflects a clear trend towards 'positive' and 'trust'. 
##However, individual terms might deliver an ambiguous message.

#Creation of bigrams
tesla_bigrams <- tesla_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

tesla_bigrams %>%
count(bigram, sort = TRUE)

tesla_separated <- tesla_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

tesla_filtered <- tesla_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stop$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word2 %in% custom_stop$word) 

tesla_bigram_counts <- tesla_filtered %>%
  count(word1, word2, sort = TRUE)

#Visualizing the bigram network
tesla_bigram_graph <- tesla_bigram_counts %>%
  filter(n>1) %>% #DECREASE THIS TO N>1 FOR NLP SURVEY
  graph_from_data_frame()

ggraph(tesla_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
