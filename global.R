# Load required packages
library(dplyr)
library(future)
library(httr)
library(lubridate)
library(mongolite)
library(newspapr)
library(RCurl)
library(RJSONIO)
library(readability)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(stringr)
library(tidyr)
library(tidytext)

# Setting up asynchronous programming
future::plan(multiprocess)

# Links
newsapi_url <- a("NewsAPI", href = "https://newsapi.org/register")
diffbot_url <- a("Diffbot API", href = "https://www.diffbot.com/plans/trial")

# Head
head <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
)

# MongoDB connection URL
url <- paste0("mongodb://admin:example@", # authentication
              "newsrec-shard-00-00-t0scv.mongodb.net:27017,", # host 1 & port
              "newsrec-shard-00-01-t0scv.mongodb.net:27017,", # host 2 & port
              "newsrec-shard-00-02-t0scv.mongodb.net:27017", # host 3 & port
              "/test?", 
              "ssl=true",
              "&replicaSet=newsrec-shard-0",
              "&authSource=admin",
              "&retryWrites=true",
              "&w=majority")

# Function to check for valid user
.verify <- function(user, pass) {
  con <- mongo(collection = "users", db = "users", url = url)
  users <- con$find()
  if (user %in% users$username) {
    current_user <- users %>%
      filter(username == user)
    if (current_user$password == pass) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Function to register a user
.register <- function(user, pass, news_key, diffbot_key) {
  # Add user to 'users' collection
  con <- mongolite::mongo(collection = "users", db = "users", url = url)
  user <- data.frame(username = user, password = pass, NEWS_API_KEY = news_key, DIFFBOT_API_KEY = diffbot_key)
  con$insert(user)
}

.set_keys <- function(user, pass) {
  con <- mongo(collection = "users", db = "users", url = url)
  users <- con$find()    
  current_user <- users %>%
    filter(username == user)
  Sys.setenv(NEWS_API_KEY = current_user$NEWS_API_KEY)
  Sys.setenv(DIFFBOT_API_KEY = current_user$DIFFBOT_API_KEY)
}

# Function to check if a username exists
.username_exists <- function(u) {
  con <- mongolite::mongo(collection = "users", db = "users", url = url)
  users <- con$find()
  if (u %in% users$username) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

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

################################################################################

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
.get_sentiments <- function(text) {
  text_tbl <- tibble(article = text) %>%
    tidytext::unnest_tokens(word, article) %>%
    dplyr::anti_join(stop_words)
  num_words <- nrow(text_tbl)
  nrc_scores <- text_tbl %>%
    inner_join(get_sentiments("nrc")) %>%
    group_by(sentiment) %>% 
    summarize(percentage = n() / num_words) %>%
    spread(key = sentiment, value = percentage)
  return(nrc_scores)
}

################################################################################

# Login page
login_page <- div(
  id = "login-page",
  wellPanel(
    tags$h2("Log In or Sign Up", class = "text-center"),
    textInput(inputId = "username", 
              placeholder = "Username", 
              label = tagList(icon("user"), "Username")),
    passwordInput(inputId = "password", 
                  placeholder = "Password", 
                  label = tagList(icon("unlock-alt"), "Password")),
    tagList("If you're signing up, you also need to provide valid API keys for", 
            newsapi_url, "and", diffbot_url, "in the fields below."),
    br(),
    br(),
    textInput(inputId = "newsapi", 
              placeholder = "NewsAPI Key", 
              label = tagList(icon("key"), "NewsAPI Key")),
    textInput(inputId = "diffbot", 
              placeholder = "Diffbot API Key", 
              label = tagList(icon("key"), "Diffbot API Key")),
    div(
      class = "text-center",
      div(
        actionButton(inputId = "login", class = "button", "LOG IN"),
        actionButton(inputId = "signup", class = "button", "SIGN UP"),
      ),
      br(),
      shinyjs::hidden(
        div(
          id = "incorrect",
          p(class = "text-center error", "Incorrect username or password.")
        ),
        div(
          id = "existing",
          p(class = "text-center error", "Username already exists.")
        ),
        div(
          id = "missing",
          p(class = "text-center error", "Missing API key(s).")
        ),
        div(
          id = "invalid",
          p(class = "text-center error", "Invalid API key(s).")
        ),
        div(
          id = "wait",
          p(class = "text-center error", "Checking API keys... Please wait.")
        )
      )
    )
  )
)

# Training page
train_page <- tabItem(
  tabName = "train",
  class = "active",
  h1("Train the System"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("This page is where you train the system. This means that you tell the system whether
        or not you like certain articles, and then the system gives you output based on that."),
      textInput(inputId = "keyword", 
                label = "Please enter some keywords to generate a training set of articles",
                placeholder = "e.g. Joe Biden"),
      actionButton(inputId = "generate",
                   label = "Generate Training Set")
    ),
    mainPanel(
      uiOutput("article")
    )
  )
)

# Explore page
explore_page <- tabItem(
  tabName = "explore",
  h1("Explore Recommended News")
)

# How It Works page
how_page <- tabItem(
  tabName = "how",
  h1("How the System Works")
)

# About page
about_page <- tabItem(
  tabName = "about",
  h1("About the System")
)

# #############################################################################
#
# Experimental code for web scraping, to eliminate dependency on Diffbot
# !!! DO NOT EDIT OR UNCOMMENT !!!
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
# TO DO: split and merge list to get only main content... but how?
#
# #############################################################################
