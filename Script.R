#' ---
#' title: "McDonald's Review Analysis"
#' author: " "
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      
#'     highlight: kate      
#'     toc: true         
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false
#' ---


#' # Setup

# Set working directory
#setwd("Desktop/Coding/mcdonalds-review-analysis")

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)

library(tm)
library(wordcloud)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggthemes)

# LDA function
top_terms_by_topic_LDA <- function(input_text, 
                                   k = 3,
                                   plot = TRUE) 
{    
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  

  unique_indexes <- unique(DTM$i) 
  DTM <- DTM[unique_indexes,]    

  lda <- LDA(DTM, k , control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  

  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) 
  
  
  
  if(plot == T){
    top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") + 
      labs(x = "Terms", y = "β (word importance in topic)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  }else{ 
    return(top_terms)
  }
  
  
}

#' # Data preparation
# Load data
data <- read.csv("data/McDonald_s_Reviews.csv")

# Create corpus                
corpus <- VCorpus(VectorSource(data$review))


# Ensure UTF-8 encoding
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

# Remove non ASCII characters
corpus <- tm_map(corpus, content_transformer(function(x) {
  gsub("[^\x01-\x7F]", "", x) 
}))

# View content
corpus[[1]][[1]][1:2]

# Convert pattern to whitespace
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Remove tab
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# Transform conent to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

# Custom stopwords
custom_stop_words <- c(
  "mcdonalds", "mcdonald", "mc", "mcd",
  "food", "fast", "burger", "fries",
  "restaurant", "order", "ordered", "got",
  "went", "place", "time", "just", "really"
)

# Remove unnecessary characters/words
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
courpus <- tm_map(corpus, removeWords, custom_stop_words)
corpus <- tm_map(corpus, stripWhitespace)


corpus[[1]][[1]][1:2]

# TDM Matrix
tdm_tfidf <- TermDocumentMatrix(corpus,
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))


# Sort words by frequency
v <- sort(rowSums(as.matrix(tdm_tfidf)), decreasing = TRUE)
tdm_tfidf <- data.frame(word = names(v), freq = v)

# Get first 100 words for faster processing
tdm_tfidf <- head(tdm_tfidf, 100) 

#' # Word frequency count
top_terms <- head(tdm_tfidf, 10)
head(tdm_tfidf, 10)

# Create a word frequency count plot
ggplot(top_terms, aes(x = reorder(word, freq), y = freq)) +
  geom_col(fill = "darkgreen") + 
  coord_flip() +
  labs(x = "Term", y = "Total TF-IDF Score") +
  theme_minimal()

#' # Word cloud
wordcloud(
  words = tdm_tfidf$word,
  freq = tdm_tfidf$freq,
  colors = brewer.pal(8, "Dark2")
)

#' # LDA

# Set number of topics
number_of_topics = 2
top_terms_by_topic_LDA(tdm_tfidf$word, k = number_of_topics)


#' # Sentiment

# Prepare data for sentiment review
sentiment_review <- data %>%
  select(review) %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, review) %>%     
  anti_join(stop_words, by = "word") %>%    
  inner_join(get_sentiments("loughran"),   
             by = "word")

# Filter for only postivie and negatvive 
sentiment_review2 <- sentiment_review %>%
  filter(sentiment %in% c("positive", "negative"))

# Counts word per sentiment
word_counts <- sentiment_review2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Create a plot
ggplot(word_counts, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(x = "Words", y = "Count") +
  theme_gdocs() +
  ggtitle("Words count per sentiment (Loughran)") +
  scale_fill_manual(values = c("firebrick", "darkolivegreen4"))


