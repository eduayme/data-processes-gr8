#Set WorkSpece
rm(list = ls())
#setwd("D:/AplicacionesInformaticas/workspaceR")

#Load DataSet
Sys.setlocale("LC_ALL","English")


#Install and library packages
#install.packages("lattice")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("ggvis")
#install.packages('DMwR')
#install.packages('arules')
#install.packages('party')


library(lattice)
library(ggplot2)
library(caret)
library(dplyr)
library(rpart)
library(stringr)
library(rpart.plot)
library(ggvis)
library(grid)
library(DMwR)
library(Matrix)
library(arules)

# PREPROCESSING DATA

Movies <- read.csv(file="./data/tmdb_5000_movies.csv" , header = TRUE , sep = ",")
Movies$spoken_languages <- NULL
Movies$title <- NULL
Movies$homepage <- NULL
Movies$overview <- NULL
Movies$keywords <- NULL
Movies$genres <- NULL
Movies$production_countries <- NULL
Movies$production_companies <- NULL
Movies$id <- NULL

Movies <- Movies %>%
  mutate(original_language = factor(original_language), )


Movies <- Movies %>%
  mutate(release_date = as.Date(release_date), )
##we wont consider either the movies with budget or revenue that equals 0, as we suppose that this is an error.
Movies$name_length <- str_length(Movies$original_title)
Movies$original_title <- NULL

Movies$tagline_length <- str_length(Movies$tagline)
Movies$tagline <- NULL

Movies <- subset(Movies,  popularity!=0 & revenue!=0 & vote_count!=0)
head(Movies)

##Create a data frame from Movies Dataset
dfMovies <- data.frame(Movies)
  
##Using a grid search to find the best parameters for you model of interest
##At the first,we should use the function of package "caret" to create test data and train data.
#Simple random sampling
set.seed(123)

trainIndex <- createDataPartition(
  dfMovies$vote_average, # Sample proportionally based on the outcome variable
  p = .8, # Percentage to be used for training
  list = FALSE, # Return the indices as a vector (not a list)
  times = 1 # Only create one set of indices
)

# Subset your data into training and testing set
training_set <- Movies[ trainIndex, ] # Use indices to get training data
test_set <- Movies[ -trainIndex, ] # Remove train indices to get test data

# Specify cross validation approach: 10 fold CV
fitControl <- trainControl(
  method = "cv", # Cross validation
  number = 10 # 10 fold
)

cat("\014") 

# -----------------------HASTA AQUÍ BIEN---------------------------------------
#

#2. Does the duration of film influence the critic score?
linearMod <- lm(vote_average ~ runtime, data=training_set)
distPred <- predict(linearMod, test_set)

actuals_preds <- data.frame(cbind(actuals=test_set$vote_average, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
regr.eval(actuals_preds$actuals, actuals_preds$predicteds)


#4. Have old films better critic scores than recent ones?
linearMod <- lm( vote_average ~ release_date, data=training_set)
distPred <- predict(linearMod, test_set)

actuals_preds <- data.frame(cbind(actuals=test_set$vote_average, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
regr.eval(actuals_preds$actuals, actuals_preds$predicteds)


#1. What attributes makes the film better (with better we refer to the money earned, the views, the ratings)?
#6. The release date influences in the popularity and revenue of a movie?

#Create a grid of parameters to search
grid <- expand.grid(k=1:20)

fit_cv_grid_revenue <- train(popularity~.,
                             data = training_set,
                             method = "knn",
                             trControl = fitControl,
                             tuneGrid = grid)

# Make predictions on the test set
preds_cv_grid <- predict(fit_cv_grid_revenue, test_set)

# Assess performance via a confusion matrix
confusionMatrix(test_set$popularity, preds_cv_grid, positive = "M")

# Show the average performance (across folds) for each value of `k`
ggplot(data = preds_cv_grid$results) +
  geom_line(mapping = aes(x = k, y = Accuracy))


