server <- function(input, output) {
  
  # URL for gif to display when system is waiting
  gif_url <- "https://gifimage.net/wp-content/uploads/2017/01/Waiting-GIF-Image-for-Whatsapp-and-Facebook-32-1.gif"
  
  # Generate training set based on input keyword
  training_set <- eventReactive(input$generate, {
    get_top_headlines(keyword = input$keyword, page_size = 100) %>%
      mutate(author = tidyr::replace_na(author, "Unknown"),
             source = tidyr::replace_na(source, "Unknown"),
             published_at = parse_date_time(published_at, orders = "ymd-HMS") %>%
               {stamp("Jan 1, 2000 at 22:10")(.)})
  })
  
  # To update the article everytime the user rates it, we need a counter
  # to run through all the articles in the training set
  counter <- reactiveValues(i = 1)
  
  observeEvent(input$rate, {
    choice <- ifelse(input$like == "Yes", 1, 0)
    article_data <- cbind(current_article(), liked = choice)
    con <- mongolite::mongo(collection = input$username, db = "preferences", url = url)
    con$insert(article_data)
    
    counter$i <- counter$i + 1
  })
  
  current_article <- reactive({
    training_set()[counter$i, ]
  })
  
  USER <- reactiveValues(logged_in = FALSE)
  
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
          NEWSAPI_KEY <- isolate(input$newsapi)
          DIFFBOT_KEY <- isolate(input$diffbot)
          shinyjs::toggle(id = "wait", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(3000, shinyjs::toggle(id = "wait", anim = TRUE, time = 1, animType = "fade"))
          if (.username_exists(username)) {
            shinyjs::toggle(id = "existing", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "existing", anim = TRUE, time = 1, animType = "fade"))
          } else if (is.null(NEWSAPI_KEY) || is.null(DIFFBOT_KEY)) {
            shinyjs::toggle(id = "missing", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "missing", anim = TRUE, time = 1, animType = "fade"))
          } else if (newspapr::check_key(NEWSAPI_KEY) != "All OK." || .check_diffbot(DIFFBOT_KEY) != "All OK.") {
            shinyjs::toggle(id = "invalid", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "invalid", anim = TRUE, time = 1, animType = "fade"))
          } else{
            .register(username, password, NEWSAPI_KEY, DIFFBOT_KEY)
            USER$logged_in <- TRUE
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
        menuItem("Explore", tabName = "explore", icon = icon("newspaper")),
        menuItem("How It Works", tabName = "how-it-works", icon = icon("question-circle")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$logged_in) {
      tabItems(
        train_page,
        tabItem(
          tabName = "explore",
          class = "active",
          h2("Explore Recommended News")
        ),
        tabItem(
          tabName = "how-it-works",
          class = "active",
          h2("How It Works")
        ),
        tabItem(
          tabName = "about",
          class = "active",
          h2("About")
        )
      )
    } else {
      login_page
    }
  })
  
  output$article <- renderUI({
    if (input$keyword != "" && input$generate > 0) {
      if (counter$i <= nrow(training_set())) {
        wellPanel(
          h2(current_article()$title),
          h5(paste("Source:", current_article()$source)),
          h5(paste("Author:", current_article()$author)),
          fluidRow(
            column(
              width = 6,
              img(src = current_article()$url_to_image, height = "100%", width = "100%", align = "right")
            ),
            column(
              width = 6,
              p(current_article()$description),
              p(paste("Published:", current_article()$published_at))
            )
          ),
          hr(),
          div(
            class = "text-center",
            radioButtons(inputId = "like", label = "Do you like this article?",
                         choices = c("Yes", "No"), inline = TRUE)
          ),
          div(
            class = "text-center",
            actionButton(inputId = "rate", label = "Submit Rating")
          )
        )
      } else {
        column(
          width = 12,
          h2("Please enter another keyword to search."),
          img(src = gif_url, height = "100%", width = "100%", align = "right")
        )
      }
    } else {
      img(src = gif_url, height = "100%", width = "100%", align = "right")
    }
  })
}
