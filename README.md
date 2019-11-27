
<!-- README.md is generated from README.Rmd -->

# News Recommendation System

A news recommendation system written in R and deployed as an online
Shiny application.

## Usage

To use the system, you must first create an account, optionally
specifying preferences such as language. When you log in for the first
time, the system will generate a random sample of current news articles
from a variety of sources. You must train the system by marking each
article as a “Like” or as a “Dislike”. All of your choices will be
remembered, unless you choose to flush the system and start again. The
longer you train the system, the more well-tailored the recommendations
will be to your taste.

When you’re done training the system (we recommend rating at least 10-20
articles), you will be issued with a list of recent news articles. As
you read through them, you can also mark these with a “Like” or
“Dislike”, and the system will use your input to improve its
recommendations in the future.
