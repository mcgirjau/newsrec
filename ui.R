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
        )
      )
    )
  )
  
)

header <- dashboardHeader(title = "News Recommender", uiOutput("logout"))

# sidebar <- dashboardSidebar(uiOutput("sidebar"), collapsed = TRUE)
sidebar <- dashboardSidebar(collapsed = TRUE)

body <- dashboardBody(head, shinyjs::useShinyjs(), login_page)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "black"
)