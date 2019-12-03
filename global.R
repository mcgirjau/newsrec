# #############################################################################
#
# SETUP
#
# #############################################################################

# Load required packages
library(dplyr)
library(future) # asynchronous programming
library(future.apply) # asynchronous programming (apply functions)
library(httr)
library(lubridate)
library(mongolite) # MongoDB connection
library(newspapr) # NewsAPI wrapper package
library(RCurl)
library(readability) # readability scores
library(RJSONIO)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(stringr)
library(tidyr)
library(tidytext)

# Setting up asynchronous programming so that the app doesn't lag
future::plan(multiprocess)

# Links to where one can obtain API keys for NewsAPI and Diffbot
newsapi_url <- a("NewsAPI", href = "https://newsapi.org/register")
diffbot_url <- a("Diffbot API", href = "https://www.diffbot.com/plans/trial")

# Link to the CSS style sheet (in www/ folder)
head <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
)

# MongoDB connection URL - DO NOT EDIT
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

# #############################################################################
#
# UTILITY FUNCTIONS
#
# #############################################################################

# Function to check if a username already exists in the MongoDB database
.username_exists <- function(u) {
  con <- mongolite::mongo(collection = "users", db = "users", url = url)
  users <- con$find()
  if (u %in% users$username) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Function to check for valid user in MongoDB database
.verify <- function(u, p) {
  con <- mongolite::mongo(collection = "users", db = "users", url = url)
  users <- con$find()
  if (u %in% users$username) {
    current_user <- users %>%
      filter(username == u)
    if (current_user$password == p) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Function to register a user in MongoDB database
.register <- function(u, p, news_key, diffbot_key) {
  # Add user to 'users' collection
  con <- mongolite::mongo(collection = "users", db = "users", url = url)
  user <- data.frame(
    username = u, 
    password = p, 
    NEWS_API_KEY = news_key, 
    DIFFBOT_API_KEY = diffbot_key
  )
  con$insert(user)
}

# Function to set the current user's API keys as environment variables
.set_environment <- function(u) {
  con <- mongo(collection = "users", db = "users", url = url)
  users <- con$find()    
  current_user <- users %>%
    filter(username == u)
  Sys.setenv(USERNAME = u)
  Sys.setenv(NEWS_API_KEY = current_user$NEWS_API_KEY)
  Sys.setenv(DIFFBOT_API_KEY = current_user$DIFFBOT_API_KEY)
}

# Function to check the validity of the API keys
.valid_keys <- function(news_key, diffbot_key) {
  # Mock URL to test the Diffbot API key
  mock_url <- "https://www.cnn.com/2019/11/26/tech/google-employee-tensions/index.html"
  query_url <- paste0("http://api.diffbot.com/v2/article?token=", diffbot_key, "&url=", mock_url)
  outcome <- httr::GET(query_url) %>%
    httr::content()
  if (!is.null(outcome$errorCode)) {
    return(FALSE)
  }
  # Using newspapr package to test NewsAPI key
  result <- tryCatch({
    newspapr::check_key(news_key)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
  return(result)
}

# Function to submit preferences to the MongoDB database
.submit <- function(text, liked) {
  if (!is.null(text)) {
    article_data <- bind_cols(opinion = liked, 
                              readability = .get_readability(text),
                              length = .get_length(text),
                              afinn_score = .get_sentiment(text)) %>%
      mutate_all(replace_na, 0)
    # Submit to database only if content isn't empty
    con <- mongolite::mongo(collection = Sys.getenv("USERNAME"), db = "preferences", url = url)
    con$insert(article_data)
  }
}

# #############################################################################
#
# TEXT METRICS
#
# #############################################################################

# Function to get article text content using Diffbot API (REQUIRES KEY)
.get_text <- function(article_url) {
  DIFFBOT_KEY <- Sys.getenv("DIFFBOT_API_KEY")
  query_url <- paste0("http://api.diffbot.com/v2/article?token=", DIFFBOT_KEY, 
                      "&url=", article_url)
  text <- query_url %>%
    RCurl::getURL() %>%
    RJSONIO::fromJSON() %>%
    as.list() %>%
    .$text
  if (!is.null(text) && text == "") {
    text <- NULL
  }
  return(text)
}

# Function to get Flesch-Kincaid Grade Level of article (readability metric)
.get_readability <- function(text) {
  flesch_kincaid <- readability::readability(text, grouping.var = TRUE) %>%
    .$Flesch_Kincaid
  return(flesch_kincaid)
}

# Function to get length of article (in number of characters)
.get_length <- function(text) {
  return(nchar(text))
}

# Function to get sentiments (based on AFINN lexicon) of article
.get_sentiment <- function(text) {
  # Clean up text and remove stop words
  text_tbl <- tibble(article = text) %>%
    tidytext::unnest_tokens(word, article) %>%
    dplyr::anti_join(stop_words)
  num_words <- nrow(text_tbl)
  afinn_score <- text_tbl %>%
    inner_join(tidytext::get_sentiments("afinn")) %>%
    summarize(afinn_score = sum(value)) %>%
    .$afinn_score
  return(afinn_score)
}

# Function to get metrics for a data frame of articles
.get_metrics <- function(articles) {
  metrics_list <- list()
  # Use asychronous programming
  metrics_list <- future.apply::future_lapply(1:nrow(articles), function(row) {
    current_article <- articles[row, ]
    text <- .get_text(current_article$url)
    if (!is.null(text)) {
      current_metrics <- current_article %>%
        mutate(readability = .get_readability(text),
               length = .get_length(text),
               afinn_score = .get_sentiment(text)) %>%
        select(readability, length, afinn_score)
    } else {
      current_metrics <- current_article %>%
        mutate(readability = NA,
               length = NA,
               afinn_score = NA) %>%
        select(readability, length, afinn_score)
    }
    return(current_metrics)
  })
  # Convert to data frame
  all_metrics <- do.call(rbind.data.frame, metrics_list)
  return(all_metrics)
}

# #############################################################################
#
# UI COMPONENTS
#
# #############################################################################

# Login page
login_page <- div(
  id = "login-page", # for the CSS file
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
      # Potential login errors
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
        # Message to display as keys are being checked
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
      p("This page is where you train the system. This means that you tell the 
        system whether or not you like certain articles, and then the system 
        gives you output based on that."),
      textInput(inputId = "train_keyword", 
                label = "Please enter some keywords",
                placeholder = "e.g. Joe Biden"),
      div(
        class = "text-center",
        actionButton(inputId = "generate", label = "Generate Training Set"),
        br(),
        br(),
        # Potential login errors
        shinyjs::hidden(
          div(
            id = "notfound",
            p(class = "text-center error", "No news found for this keyword.")
          )
        )
      )
    ),
    mainPanel(
      uiOutput("article")
    )
  )
)

# Explore Recommendations page
explore_page <- tabItem(
  tabName = "explore",
  h1("Explore Recommended News"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("This is where you get your recommendations"),
      selectInput(inputId = "rectype", label = "News Type", 
                  choices = c("Top Headlines", "All Articles"),
                  selected = "Top Headlines"),
      hr(),
      h3("Filters"),
      uiOutput("controls"),
      div(
        class = "text-center",
        actionButton(inputId = "give", label = "Give Me Recommendations"),
        br(),
        br(),
        shinyjs::hidden(
          div(
            id = "inputs_all",
            p(class = "text-center error", "Please enter a keyword to get recommendations.")
          ),
          div(
            id = "inputs_top",
            p(class = "text-center error", "Please enter a keyword, a country, or a category.")
          )
        )
      )
    ),
    mainPanel(
      withSpinner(uiOutput("recommend"), type = 1, color = "purple")
    )
  )
)

# How It Works page
how_page <- tabItem(
  tabName = "how",
  h1("How the System Works")
)

# About page
about_page <- tabItem(
  tabName = "about",
  h1("About the System"),
  p("Made by M.C. Girjau and S. Gurung")
)

# #############################################################################
#
# MODEL
#
# #############################################################################

# Logistic regression model to predict if user will like article or not
.fit_model <- function() {
  username <- Sys.getenv("USERNAME")
  con <- mongolite::mongo(collection = Sys.getenv("USERNAME"), db = "preferences", url = url)
  preferences <- con$find()
  model <- glm(opinion ~ ., data = preferences, family = binomial(logit),
               na.action = na.exclude)
  return(model)
}

# Function to give recommendations
# .recommend

# #############################################################################
#
# EXPERIMENTAL CODE - please disregard
#
# #############################################################################

# #############################################################################
#
# # Experimental code for web scraping, to eliminate dependency on Diffbot
# # !!! DO NOT EDIT OR UNCOMMENT !!!
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
