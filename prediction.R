rm(list = ls())


library(dplyr)
library(ggplot2)
library(tibble)
library(caret)
library(stringr)

getwd();
movies <- read.csv("data/tmdb_5000_movies.csv", stringsAsFactors = FALSE)


# PREPROCESSING

movies$spoken_languages <- NULL
movies$title <- NULL
movies$homepage <- NULL
movies$overview <- NULL
movies$keywords <- NULL

movies <- movies %>%
  mutate(original_language = factor(original_language), )

movies$name_length <- str_length(movies$original_title)
movies$original_title <- NULL

movies$tagline_length <- str_length(movies$tagline)
movies$tagline <- NULL

movies <- subset(movies,  popularity!=0 & revenue!=0 & vote_count!=0)
head(movies)

set.seed(123)

# First question: What attributes makes the film better (with better we refer to the money earned, the views, the ratings)?

table(movies$vote_average)

trainIndex <- createDataPartition(
  movies$vote_average, # Sample proportionally based on the outcome variable
  p = .8, # Percentage to be used for training
  list = FALSE, # Return the indices as a vector (not a list)
  times = 1 # Only create one set of indices
)

# Subset your data into training and testing set
training_set <- movies[ trainIndex, ] # Use indices to get training data
test_set <- movies[ -trainIndex, ] # Remove train indices to get test data

# Specify cross validation approach: 10 fold CV
fitControl <- trainControl(
  method = "cv", # Cross validation
  number = 10 # 10 fold
)
