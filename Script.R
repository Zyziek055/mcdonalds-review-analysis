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
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)

library(tm)
library(wordcloud)
library(topicmodels)
library(tidyverse)
library(tidytext)

# LDA function
top_terms_by_topic_LDA <- function(input_text, # wektor lub kolumna tekstowa z ramki danych
                                   plot = TRUE, # domyślnie rysuje wykres
                                   k = number_of_topics) # wyznaczona liczba k tematów
{    
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  
  # usuń wszystkie puste wiersze w macierzy częstości
  # ponieważ spowodują błąd dla LDA
  unique_indexes <- unique(DTM$i) # pobierz indeks każdej unikalnej wartości
  DTM <- DTM[unique_indexes,]    # pobierz z DTM podzbiór tylko tych unikalnych indeksów
  
  # wykonaj LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") # pobierz słowa/tematy w uporządkowanym formacie tidy
  
  # pobierz dziesięć najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # uporządkuj słowa w malejącej kolejności informatywności
  
  
  
  # rysuj wykres (domyślnie plot = TRUE)
  if(plot == T){
    # dziesięć najczęstszych słów dla każdego tematu
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% # posortuj słowa według wartości beta 
      ggplot(aes(term, beta, fill = factor(topic))) + # rysuj beta według tematu
      geom_col(show.legend = FALSE) + # wykres kolumnowy
      facet_wrap(~ topic, scales = "free") + # każdy temat na osobnym wykresie
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  }else{ 
    # jeśli użytkownik nie chce wykresu
    # wtedy zwróć listę posortowanych słów
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
custom_stop_words<- c("mcdonalds", "mcdonald", "mcdonald's", "food", "fast", "burger", "fries")

# Remove unnecessary characters/words
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("en"), custom_stop_words))
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
head(tdm_tfidf, 10)

#' # Word cloud
wordcloud(
  words = tdm_tfidf$word,
  freq = tdm_tfidf$freq,
  colors = brewer.pal(8, "Dark2")
)

#' # LDA
number_of_topics = 3
top_terms_by_topic_LDA(tdm_tfidf$word)
