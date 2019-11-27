header <- dashboardHeader(title = "News Recommender", uiOutput("logout"))

sidebar <- dashboardSidebar(uiOutput("sidebar"), collapsed = TRUE)

body <- dashboardBody(head, shinyjs::useShinyjs(), uiOutput("body"))

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "black"
)
