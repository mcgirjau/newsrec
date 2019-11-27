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
          if (!(.username_exists(username))) {
            .register(username, password)
            USER$logged_in <- TRUE
          } else {
            shinyjs::toggle(id = "existing", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "existing", anim = TRUE, time = 1, animType = "fade"))
          }
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
  
  output$sidebar <- renderUI({
    if (USER$logged_in){ 
      sidebarMenu(
        menuItem("Train", tabName = "train", icon = icon("brain")),
        menuItem("Explore", tabName = "explore", icon = icon("newspaper"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$logged_in) {
      tabItem(
        tabName = "train", 
        class = "active"
      )
      tabItem(
        tabName = "explore",
        class = "active"
      )
    } else {
      login_page
    }
  })
}
