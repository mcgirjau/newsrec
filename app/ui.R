dashboard_header <- dashboardHeader(title = "News Recommender", uiOutput("logout"))

dashboard_sidebar <- dashboardSidebar(uiOutput("sidebar"), collapsed = TRUE)

dashboard_body <- dashboardBody(.head, shinyjs::useShinyjs(), uiOutput("body"))

ui <- dashboardPage(
  header = dashboard_header,
  sidebar = dashboard_sidebar,
  body = dashboard_body,
  skin = "yellow"
)
