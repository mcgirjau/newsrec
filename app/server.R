server <- function(input, output, session) {
  
  # ###########################################################################
  #
  # GENERAL
  #
  # ###########################################################################
  
  # URL for gif to display when system is waiting
  gif_url <- "https://www.losporkos.com/home/images/gify/farmar-300c.gif"
  
  # Dynamically close sidebar when a tab is clicked
  observeEvent(input$sidebar, {
    # for desktop browsers
    addClass(selector = "body", class = "sidebar-collapse")
    # for mobile browsers
    removeClass(selector = "body", class = "sidebar-open")
  })
  
  # ###########################################################################
  #
  # OVERALL LAYOUT
  #
  # ###########################################################################
  
  output$sidebar <- renderUI({
    if (USER$logged_in){ 
      sidebarMenu(
        id = "sidebar",
        menuItem("Train", tabName = "train", icon = icon("brain")),
        menuItem("Explore", tabName = "explore", icon = icon("newspaper")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$logged_in) {
      tabItems(
        .train_page,
        .explore_page,
        .about_page
      )
    } else {
      .login_page
    }
  })
  
  # ###########################################################################
  #
  # LOGIN PAGE
  #
  # ###########################################################################
  
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
            .set_environment(username)
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
          news_api_key <- isolate(input$newsapi)
          diffbot_api_key <- isolate(input$diffbot)
          if (.username_exists(username)) {
            shinyjs::toggle(id = "existing", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "existing", anim = TRUE, time = 1, animType = "fade"))
          } else if (news_api_key == "" || diffbot_api_key == "") {
            shinyjs::toggle(id = "missing", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "missing", anim = TRUE, time = 1, animType = "fade"))
          } else {
            shinyjs::toggle(id = "wait", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "wait", anim = TRUE, time = 1, animType = "fade"))
            if (!.valid_keys(news_api_key, diffbot_api_key)) {
              shinyjs::toggle(id = "invalid", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "invalid", anim = TRUE, time = 1, animType = "fade")) 
            } else {
              .register(username, password, news_api_key, diffbot_api_key)
              .set_environment(username)
              USER$logged_in <- TRUE
            } 
          }
        } 
      }
    }    
  })
  
  output$logout <- renderUI({
    req(USER$logged_in)
    tags$li(
      class = "dropdown",
      # Reload page to log out
      a(icon("sign-out-alt"), "Log Out", href = "javascript:window.location.reload(true)"),
    )
  })
  
  # ###########################################################################
  #
  # TRAINING PAGE
  #
  # ###########################################################################
  
  # Generate training set based on input keyword
  training_set <- eventReactive(input$generate, {
    news <- newspapr::get_everything(keyword = input$train_keyword, page_size = 100)
    if (nrow(news) == 0) {
      shinyjs::toggle(id = "notfound", anim = TRUE, time = 1, animType = "fade")
      shinyjs::delay(3000, shinyjs::toggle(id = "notfound", anim = TRUE, time = 1, animType = "fade"))
    } else {
      news <- news %>% 
        mutate(author = tidyr::replace_na(author, "Unknown"),
               source = tidyr::replace_na(source, "Unknown"),
               published_at = parse_date_time(published_at, orders = "ymd-HMS") %>%
                 {stamp("Jan 1, 2000 at 22:10")(.)})
    }
    return(news)
  })
  
  # To update the article everytime the user rates it, we need a counter
  # to run through all the articles in the training set
  counter <- reactiveValues(i = 1)
  
  current_article <- reactive({
    training_set()[counter$i, ]
  })
  
  # When the user rates an article, submit article info to database
  observeEvent(input$rate, {
    current_url <- current_article()$url
    liked <- ifelse(input$like == "Yes", 1, 0)
    # Using asynchronous programming
    future({
      .get_text(current_url)
    }) %...>% .submit(liked)
    counter$i <- counter$i + 1
  })
  
  ready_to_train <- eventReactive(input$generate, {
    if (input$train_keyword != "") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, ignoreNULL = FALSE)
  
  # Training articles output
  output$article <- renderUI({
    if (ready_to_train()) {
      if (counter$i <= nrow(training_set())) {
        wellPanel(
          class = "well",
          h2(a(current_article()$title, href = current_article()$url)),
          h4(paste("by", current_article()$author)),
          h4(paste("Source:", current_article()$source)),
          fluidRow(
            column(
              width = 6,
              img(src = current_article()$url_to_image, height = "100%", width = "100%", align = "right")
            ),
            column(
              width = 6,
              h4(current_article()$description),
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
          h2("Please enter another keyword to search for..."),
          img(src = gif_url, width = "80%", align = "center")
        )
      }
    } else {
      column(
        width = 12,
        h2("Please enter a keyword to search for..."),
        img(src = gif_url, width = "80%", align = "center")
      )
    }
  })
  
  # ###########################################################################
  #
  # EXPLORE PAGE
  #
  # ###########################################################################
  
  countries <- c("All", 
                 Argentina = "ar", 
                 Austria = "at", 
                 Australia = "au", 
                 Belgium = "be", 
                 Bulgaria = "bg", 
                 Brazil = "br", 
                 Canada = "ca", 
                 China = "cn",
                 Colombia = "co", 
                 Cuba = "cu", 
                 "Czech Republic" = "cz", 
                 Egypt = "eg", 
                 France = "fr", 
                 Germany = "de",
                 "Great Britain" = "gb", 
                 Greece = "gr", 
                 "Hong Kong" = "hk", 
                 Hungary = "hu",
                 Indonesia = "id", 
                 Ireland = "ie", 
                 Israel = "il", 
                 India = "in", 
                 Italy = "it", 
                 Japan = "jp", 
                 Lithuania = "lt", 
                 Latvia = "lv", 
                 Morocco = "ma",
                 Mexico = "mx", 
                 Malaysia = "my", 
                 Nigeria = "ng", 
                 Netherlands = "nl", 
                 Norway = "no", 
                 "New Zealand" = "nz", 
                 Phillipines = "ph", 
                 Poland = "pl", 
                 Portugal = "pt", 
                 Romania = "ro",
                 Russia = "ru", 
                 "Saudi Arabia" = "sa", 
                 Serbia = "rs",
                 Singapore = "sg", 
                 Slovenia = "si", 
                 Slovakia = "sk", 
                 "South Africa" = "za",
                 "South Korea" = "kr",
                 Sweden = "se",
                 Switzerland = "ch",
                 Thailand = "th", 
                 Turkey = "tr", 
                 Taiwan = "tw",
                 Ukraine = "ua", 
                 "United Arab Emirates" = "ae",
                 "United States" = "us", 
                 Venezuela = "ve")
  
  output$controls <- renderUI({
    if (input$rectype == "Top Headlines") {
      column(
        width = 12,
        textInput(inputId = "explore_keyword_top", 
                  label = "Keyword:",
                  placeholder = "e.g. Hong Kong protests"),
        selectInput(inputId = "explore_country", 
                    label = "Country:", 
                    choices = countries),
        selectInput(inputId = "explore_category", 
                    label = "Category:",
                    choices = c("All", "Business", "Entertainment", "General", 
                                "Health", "Science", "Sports", "Technology"))
      )
    } else if (input$rectype == "All Articles") {
      column(
        width = 12,
        textInput(inputId = "explore_keyword_all", 
                  label = "Keyword:",
                  placeholder = "e.g. Hong Kong protests"),
        textInput(inputId = "explore_title_keyword", 
                  label = "Title Keyword:",
                  placeholder = "e.g. China"),
        selectInput(inputId = "explore_language", 
                    label = "Language", 
                    choices = c("All", "Arabic", "Chinese", "Dutch", "English", "French", 
                                "German", "Hebrew", "Italian", "Norwegian", "Portuguese", 
                                "Russian", "Spanish", "Swedish", "Urdu"))
      )
    }
  })
  
  # If not enough input information is provided, show error message
  observeEvent(input$give, {
    if (input$rectype == "Top Headlines" && 
        all(input$explore_keyword_top == "", 
            input$explore_country == "All", 
            input$explore_category == "All")) {
      shinyjs::toggle(id = "inputs_top", anim = TRUE, time = 1, animType = "fade")
      shinyjs::delay(3000, shinyjs::toggle(id = "inputs_top", anim = TRUE, time = 1, animType = "fade"))
    } else if (input$rectype == "All Articles" && 
               all(input$explore_keyword_all == "",
                   input$explore_title_keyword == "")) {
      shinyjs::toggle(id = "inputs_all", anim = TRUE, time = 1, animType = "fade")
      shinyjs::delay(3000, shinyjs::toggle(id = "inputs_all", anim = TRUE, time = 1, animType = "fade"))
    }
  })
  
  # Are the appropriate inputs selected for the recommendations to be issued?
  ready <- eventReactive(input$give, {
    if (input$rectype == "Top Headlines") {
      if (any(input$explore_keyword_top != "",
              input$explore_country != "All",
              input$explore_category != "All")) {
        return(TRUE)
      }
    } else if (input$rectype == "All Articles") {
      if (any(input$explore_keyword_all != "",
              input$explore_title_keyword != "")) {
        return(TRUE)
      }
    }
    return(FALSE)
  }, ignoreNULL = FALSE)
  
  # Generate article pool to select recommendations from
  article_pool <- eventReactive(input$give, {
    articles <- data.frame()
    keyword <- NULL
    country <- NULL
    category <- NULL
    title_keyword <- NULL
    language <- NULL
    if (input$rectype == "Top Headlines") {
      if (input$explore_keyword_top != "") {
        keyword <- isolate(input$explore_keyword_top)
      }
      if (input$explore_country != "All") {
        country <- isolate(input$explore_country)
      }
      if (input$explore_category != "All") {
        category <- isolate(input$explore_category)
      }
      articles <- newspapr::get_top_headlines(keyword = keyword, country = country, 
                                              category = category)
    } else if (input$rectype == "All Articles") {
      if (input$explore_keyword_all != "") {
        keyword <- isolate(input$explore_keyword_all)
      }
      if (input$explore_title_keyword != "") {
        title_keyword <- isolate(input$explore_title_keyword)
      }
      if (input$explore_language != "All") {
        language <- isolate(input$explore_language)
      }
      articles <- newspapr::get_everything(keyword = keyword, title_keyword = title_keyword, 
                                           language = language)
    }
    if (nrow(articles) != 0) {
      articles <- articles %>% 
        mutate(author = tidyr::replace_na(author, "Unknown"),
               source = tidyr::replace_na(source, "Unknown"),
               published_at = parse_date_time(published_at, orders = "ymd-HMS") %>%
                 {stamp("Jan 1, 2000 at 22:10")(.)})
    }
    return(articles)
  })
  
  recommendation <- reactive({
    username <- Sys.getenv("USERNAME")
    con <- mongolite::mongo(collection = username, db = "preferences", url = .url)
    preferences <- con$find()
    model <- .fit_model()
    metrics <- .get_metrics(article_pool()) %>%
      mutate_all(as.numeric)
    # Check if system has been trained and if current metrics are valid
    if (nrow(preferences) == 0 || all(is.na(metrics))) {
      # if not, then just suggest top article from pool
      result <- cbind(article_pool(), pred_prob = NA) %>%
        head(n = 1)
      return(result)
    } else {
      predictions <- predict(model, newdata = metrics, type = "response")
      result <- bind_cols(article_pool(), metrics, pred_prob = predictions) %>%
        arrange(desc(pred_prob)) %>%
        head(n = 1)
      return(result)
    }
  })
  
  # Recommendation panel
  output$recommend <- renderUI({
    if (ready()) {
      if (nrow(article_pool()) == 0) {
        column(
          width = 12,
          h2("Search did not generate any articles. Try some other inputs..."),
          img(src = gif_url, width = "80%", align = "center")
        )
      } else {
        wellPanel(
          class = "well",
          h2(a(recommendation()$title, href = recommendation()$url)),
          h4(paste("by", recommendation()$author)),
          h4(paste("Source:", recommendation()$source)),
          fluidRow(
            column(
              width = 6,
              img(src = recommendation()$url_to_image, height = "100%", width = "100%", align = "right")
            ),
            column(
              width = 6,
              h4(recommendation()$description),
              p(paste("Published:", recommendation()$published_at))
            )
          ),
          hr(),
          if (!is.na(recommendation()$pred_prob)) {
            h4(paste("Predicted probability that you'll like this article is", 
                     round(recommendation()$pred_prob, 3)))
          }
        )
      }
    } else {
      column(
        width = 12,
        h2("Please enter some keyword to search for..."),
        img(src = gif_url, width = "80%", align = "center")
      )
    }
  })
}
