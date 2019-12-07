
<!-- README.md is generated from README.Rmd. Please edit that file. -->

<div style="text-align: justify">

# News Recommendation System

A news recommendation system written in R and deployed as an online
Shiny application.

## Requirements

### API Keys

To use the News Recommendation System, you need two API keys:

  - A [NewsAPI](https://newsapi.org/) key for pulling in news. This API
    offers a free plan, which comes with
    [limitations](https://newsapi.org/pricing), e.g. a maximum of 500
    requests per day, and new articles available with a 15-minute delay.
    A key can be obtained [here](https://newsapi.org/register).
  - A [Diffbot Article
    API](https://www.diffbot.com/products/automatic/#article) key for
    the web extraction of article contents. This API offers a 14-day
    free trial, with [limitations](https://www.diffbot.com/pricing/)
    like only 10,000 included requests, and a maximum of 1 request per
    second. A key can be obtained
    [here](https://www.diffbot.com/plans/trial), and a credit card is
    not necessary for the free trial.

### Package Dependencies

The News Recommendation System relies on a number of R packages. To run
the app, you must have all of them installed. You can ensure all
dependencies are satisfied by running the code below in your R console:

``` r
# Packages to install from CRAN
package_list <- c("devtools", "dplyr", "future", "future.apply", "httr", 
                  "lubridate", "mongolite", "promises", "RCurl", "readability", 
                  "RJSONIO", "shiny", "shinycssloaders", "shinydashboard", 
                  "shinyjs", "stringr", "tidyr", "tidytext")

missing_packages <- package_list[!(package_list %in% installed.packages()[ , "Package"])]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Packages to install from GitHub
devtools::install_github("mcgirjau/newspapr")
```

## Usage

…coming soon

</div>
