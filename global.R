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
library(sodium)
library(mongolite)

# Head
head <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
)

# Establish MongoDB connection
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
  
con <- mongo(collection = "users", db = "users", url = url)
users <- con$find()

# Function to check for valid user
.verify <- function(username, password) {
  if (username %in% users$username) {
    user <- users[username == username]
    if (user$password == password) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Function to register a user
.register <- function(username, password) {
  user <- data.frame(username = username, password = password)
  con$insert(user)
}
