################################################################################
#
# TITLE: 
#
# PURPOSE: Personalized news recommendation system in the form of a ShinyApp
#
# DESCRIPTION: Shiny app should include a login page that remembers and stores 
#              user preferences
# 
################################################################################

# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(mongolite)
library(dplyr)

# Load news recommendation engine
source("engine.R")

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
.register <- function(u, p) {
  con <- mongo(collection = "users", db = "users", url = url)
  user <- data.frame(username = u, password = p)
  con$insert(user)
}

# Function to check if a username exists
.username_exists <- function(u) {
  con <- mongo(collection = "users", db = "users", url = url)
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
    br(),
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
        )
      )
    )
  )
)
