# #############################################################################
#
# TITLE: 
#
# PURPOSE: Personalized news recommendation system in the form of a ShinyApp
#
# DESCRIPTION: Shiny app should include a login page that remembers and stores 
#              user preferences
# 
# #############################################################################

# Load required packages
library(dplyr)
library(lubridate)
library(mongolite)
library(newspapr)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(tidyr)

# Load news recommendation engine
# source("engine.R")

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
.verify <- function(u, p) {
  con <- mongo(collection = "users", db = "users", url = url)
  users <- con$find()
  if (u %in% users$username) {
    user <- users %>%
      filter(username == u)
    if (user$password == p) {
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

# Generate training set
training_set <- newspapr::get_top_headlines(keyword = "Trump")
