server <- function(input, output) {
  
  logged_in = FALSE
  USER <- reactiveValues(logged_in = logged_in)
  
  observe({ 
    if (!USER$logged_in) {
      
      # if LOG IN button has been pressed, check credentials
      if (!is.null(input$login)) {
        if (input$login > 0) {
          username <- isolate(input$username)
          password <- isolate(input$password)
          valid_credentials <- .verify(username, password)
          if (valid_credentials) {
            USER$logged_in <- TRUE
          } else {
            shinyjs::toggle(id = "incorrect", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "incorrect", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
      
      # if SIGN UP button has been pressed, add user to database and log in
      if (!is.null(input$signup)) {
        if (input$signup > 0) {
          username <- isolate(input$username)
          password <- isolate(input$password)
          .register(username, password)
          USER$logged_in <- TRUE
        } 
      }
    }    
  })
  
  output$logout <- renderUI({
    req(USER$logged_in)
    tags$li(
      class = "dropdown",
      a(icon("sign-out-alt"), "Log Out", href = "javascript:window.location.reload(true)"),
    )
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItem(tabName ="dashboard", class = "active",
              fluidRow(
                box(width = 12, dataTableOutput('results'))
              ))
    }
    else {
      loginpage
    }
  })
  
}