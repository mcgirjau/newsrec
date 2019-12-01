# #############################################################################
#
# Experimental code for web scraping - DO NOT EDIT OR UNCOMMENT
# 
# library(RCurl)
# library(XML)
# 
# url <- "https://www.cnn.com/2019/11/30/politics/donald-trump-impeachment-inquiry-strategy/index.html"
# doc <- getURL(url)
# html <- htmlTreeParse(doc, useInternal = TRUE)
# txt <- xpathApply(html, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]", xmlValue)
#
# RETURNS: list
# TO DO: split list to get only main content
#
# #############################################################################

# Load required packages
library(dplyr) # for data manipulation
library(httr)
library(RCurl) # for interacting with URLs
library(RJSONIO) # for JSON parsing
library(readability) # for readability score
library(stringr) # for string manipultion
library(tidytext) # for sentiment analysis

# Function to check Diffbot API key
.check_diffbot <- function(key) {
  url <- "https://www.cnn.com/2019/11/26/tech/google-employee-tensions/index.html"
  query_url <- paste0("http://api.diffbot.com/v2/article?token=", key, "&url=", url)
  code <- httr::GET(query_url) %>%
    httr::status_code()
  if (code == 200) {
    return("All OK.")
  } else {
    return("Error.")
  }
}

# Function to get article text content using Diffbot API (REQUIRES KEY)
.get_text <- function(url) {
  DIFFBOT_KEY <- Sys.getenv("DIFFBOT_API_KEY")
  query_url <- paste0("http://api.diffbot.com/v2/article?token=", DIFFBOT_KEY, "&url=", url)
  text <- query_url %>%
    RCurl::getURL() %>%
    RJSONIO::fromJSON() %>%
    .$text
  return(text)
}

# #############################################################################
#
# FUNCTIONS TO GET TEXT METRICS
#
# #############################################################################

# Function to get Flesch-Kincaid Grade Level (a measure of readability)
.get_readability <- function(text) {
  flesch_kincaid <- readability::readability(text, grouping.var = TRUE) %>%
    .$Flesch_Kincaid
  return(flesch_kincaid)
}

# Function to get length of article
.get_length <- function(text) {
  return(nchar(text))
}

# Function to get number of exclamation marks
.get_exclamation <- function(text) {
  exclamation <- stringr::str_count(text, pattern = "!")
  return(exclamation)
}

# Function to get sentiments (based on NRC lexicon)
.get_sentiment <- function(text) {
  
  text_tbl <- tibble(article = text) %>%
    tidytext::unnest_tokens(word, article) %>%
    dplyr::anti_join(stop_words)
  
  num_words <- nrow(text_tbl)
  
  nrc_scores <- text_tbl %>%
    inner_join(get_sentiments("nrc")) %>%
    group_by(sentiment) %>% 
    summarize(percentage = n() / num_words)
  
  return(nrc_scores)
}

