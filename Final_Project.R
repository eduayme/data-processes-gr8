#Set WorkSpece
rm(list = ls())
#setwd("D:/AplicacionesInformaticas/workspaceR")

#Load DataSet
Sys.setlocale("LC_ALL","English")
Movies <- read.csv(file="./data/tmdb_5000_movies.csv" , header = TRUE , sep = ",")

#Install and library packages
#install.packages("lattice")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("ggvis")

library(lattice)
library(ggplot2)
library(caret)
library(dplyr)
library(rpart)
library(stringr)
library(rpart.plot)
library(ggvis)

# PREPROCESSING DATA

Movies$spoken_languages <- NULL
Movies$title <- NULL
Movies$homepage <- NULL
Movies$overview <- NULL
Movies$keywords <- NULL
Movies$genres <- NULL
Movies$production_countries <- NULL
Movies$production_companies <- NULL

Movies <- Movies %>%
  mutate(original_language = factor(original_language), )
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
training_set <- movies[ trainIndex, ] # Use indices to get training data
test_set <- movies[ -trainIndex, ] # Remove train indices to get test data

# Specify cross validation approach: 10 fold CV
fitControl <- trainControl(
  method = "cv", # Cross validation
  number = 10 # 10 fold
)



# -----------------------HASTA AQUÍ BIEN---------------------------------------

#Specify cross validation approach:10 fold CV
fitControl <- trainControl(method = "cv",
                           number = 10)
#Create a grid of parameters to search
grid <- expand.grid(k=1:20)

##1.What attributes makes the film better (with better we refer to the money earned, the views, the ratings)?
#1.1 about the earned money
fit_cv_grid_revenue <- train(popularity~revenue,
                        data = test_data,
                        preProcess = "range",
                        method = "knn",
                        trControl = fitControl,
                        tuneGrid = grid)
fit_cv_grid_revenue
plot(test_data$popularity,test_data$revenue,
abline(lm(test_data$popularity~test_data$revenue),cex=1.3,
       pch = 16,xlab = "popularity",ylab="revenue"))


#1.2 about the views
fit_cv_grid_views <- train(popularity~vote_count,
                             data = test_data,
                             preProcess = "range",
                             method = "knn",
                             trControl = fitControl,
                             tuneGrid = grid)
fit_cv_grid_views
plot(test_data$popularity,test_data$vote_count,
     abline(lm(test_data$popularity~test_data$vote_count),cex=1.3,
            pch = 16,xlab = "popularity",ylab="vote_count"))

#1.3 about the ratings
fit_cv_grid_ratings <- train(popularity~vote_average,
                             data = test_data,
                             preProcess = "range",
                             method = "knn",
                             trControl = fitControl,
                             tuneGrid = grid)
fit_cv_grid_ratings
plot(test_data$popularity,test_data$vote_average,
     abline(lm(test_data$popularity~test_data$vote_average),cex=1.3,
            pch = 16,xlab = "popularity",ylab="vote_average"))


##2.Does the duration of film influence the critic score?
fit_cv_grid_runtime <- train(vote_average~runtime,
                             data = test_data,
                             preProcess = "range",
                             method = "knn",
                             trControl = fitControl,
                             tuneGrid = grid)
plot(test_data$vote_average,test_data$runtime,
     abline(lm(test_data$vote_average~test_data$runtime),cex=1.3,
            pch = 16,xlab = "vote_average",ylab="runtime"))
##3.Are some film genres more popular than others? Have it changed during years?
releaseYear <- str_split(test_data$release_date,"-")
dfReYear <- data.frame(releaseYear)
dfReYear <- dfReYear[-2,]
dfReYear <- dfReYear[-2,]
names(dfReYear) <- c(1:300)
dfReYear <- data.frame(t(dfReYear))
names(dfReYear) <- c("Year")
test_data <- cbind(dfReYear,test_data)
fit_cv_grid_releasedate <- train(genres~Year,
                             data = test_data,
                             method = "knn",
                             trControl = fitControl,
                             tuneGrid = grid)
plot(test_data$genres,test_data$Year,
     abline(lm(test_data$genres~test_data$Year),cex=1.3,
            pch = 16,xlab = "genres",ylab="Year"))

##4.Have old films better critic scores than recent ones?
fit_cv_grid_TimeAndScore <- train(vote_average~Year,
                                 data = test_data,
                                 method = "knn",
                                 trControl = fitControl,
                                 tuneGrid = grid)
plot(test_data$vote_average,test_data$Year,
     abline(lm(test_data$vote_average~test_data$Year),cex=1.3,
            pch = 16,xlab = "vote_average",ylab="Year"))

##5.Can a film earn a lot of money if the genre is not really popular in that year?
fit_cv_grid_EarnedAndPopularity <- train(revenue~popularity,
                                 data = test_data,
                                 preProcess = "range",
                                 method = "knn",
                                 trControl = fitControl,
                                 tuneGrid = grid)
plot(test_data$revenue,test_data$popularity,
     abline(lm(test_data$revenue~test_data$popularity),cex=1.3,
            pch = 16,xlab = "revenue",ylab="popularity"))

##6.The release date influences in the popularity and revenue of a movie?
##Release date and Popularity
fit_cv_grid_ReleaseAndPopularity <- train(popularity~Year,
                                         data = test_data,
                                         method = "knn",
                                         trControl = fitControl,
                                         tuneGrid = grid)
plot(test_data$popularity,test_data$Year,
     abline(lm(test_data$popularity~test_data$Year),cex=1.3,
            pch = 16,xlab = "popularity",ylab="Year"))

##Release date and Revenue
fit_cv_grid_ReleaseAndRevenue <- train(revenue~Year,
                                          data = test_data,
                                          method = "knn",
                                          trControl = fitControl,
                                          tuneGrid = grid)

plot(test_data$revenue,test_data$Year,
     abline(lm(test_data$revenue~test_data$Year),cex=1.3,
            pch = 16,xlab = "revenue",ylab="Year"))

##Employing the algorithm of interest
##1.What attributes makes the film better (with better we refer to the money earned, the views, the ratings)?
#1.1 about the earned money
Revenue <- cbind(test_data$popularity,test_data$revenue)
dfRevenue <- data.frame(Revenue)
names(dfRevenue) <- c("popularity","revenue")
RevenueFit <- rpart(revenue~.,data=dfRevenue,method = "class")
summary(RevenueFit)
popularityNewData <- data.frame(popularity = dfRevenue$popularity)
Revenuepredicted <- predict(RevenueFit,newdata = popularityNewData)
rpart.plot(RevenueFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)


#1.2 about the views
Views <- cbind(test_data$popularity,test_data$vote_count)
dfViews <- data.frame(Views)
names(dfViews) <- c("popularity","Views")
ViewsFit <- rpart(Views~.,data=dfViews,method = "class")
summary(ViewsFit)
popularityNewData <- data.frame(popularity = dfViews$popularity)
Viewspredicted <- predict(ViewsFit,newdata = popularityNewData)
rpart.plot(ViewsFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)


#1.3 about the ratings
Ratings <- cbind(test_data$popularity,test_data$vote_average)
dfRatings <- data.frame(Ratings)
names(dfRatings) <- c("popularity","Ratings")
RatingsFit <- rpart(Ratings~.,data=dfRatings,method = "class")
summary(RatingsFit)
popularityNewData <- data.frame(popularity = dfRatings$popularity)
Ratingspredicted <- predict(RatingsFit,newdata = popularityNewData)
rpart.plot(RatingsFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)


##2.Does the duration of film influence the critic score?
DurantionAndScore <- cbind(test_data$vote_average,test_data$runtime)
dfDurAndScor <- data.frame(DurantionAndScore)
names(dfDurAndScor) <- c("vote_average","runtime")
DurAndScorFit <- rpart(runtime~.,data = dfDurAndScor,method = "class")
summary(DurAndScorFit)
ScoreNewData <- data.frame(vote_average = dfDurAndScor$vote_average)
DurAndScorPredicted <- predict(DurAndScorFit,newdata = ScoreNewData)
rpart.plot(DurAndScorFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)


##3.Are some film genres more popular than others? Have it changed during years?
PopularityAndTime <- cbind(test_data$genres,test_data$Year)
dfPopuAndTime <- data.frame(PopularityAndTime)
names(dfPopuAndTime) <- c("genres","Year")
PopuAndTimeFit <- rpart(Year~.,data = dfPopuAndTime,method = "class")
summary(PopuAndTimeFit)
popularityNewData2 <- data.frame(genres = dfPopuAndTime$genres)
PopuAndTimePredicted <- predict(PopuAndTimeFit,newdata = popularityNewData2)
rpart.plot(PopuAndTimeFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)


##4.Have old films better critic scores than recent ones?
RatingsAndYear <- cbind(test_data$vote_average,test_data$Year)
dfRatAndYear <- data.frame(RatingsAndYear)
names(dfRatAndYear) <- c("vote_average","Year")
RatingAndYearFit <- rpart(Year~.,data = dfRatAndYear,method = "class")
summary(RatingAndYearFit)
RatingNewData <- data.frame(vote_average = dfRatAndYear$vote_average)
RatingAndYearPredicted <- predict(RatingAndYearFit,newdata = RatingNewData)
rpart.plot(RatingAndYearFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)


##5.Can a film earn a lot of money if the genre is not really popular in that year?
RevenueAndPopu <- cbind(test_data$revenue,test_data$popularity)
dfReveAndPopu <- data.frame(RevenueAndPopu)
names(dfReveAndPopu) <- c("revenue","popularity")
RevenueAndPopuFit <- rpart(popularity~.,data = dfReveAndPopu,method = "class")
summary(RevenueAndPopuFit)
RevenueNewData <- data.frame(revenue = dfReveAndPopu$revenue)
RevenueAndPopuPredicted <- predict(RevenueAndPopuFit,newdata = RevenueNewData)
rpart.plot(RevenueAndPopuFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)


##6.The release date influences in the popularity and revenue of a movie?
##Release date and Popularity
PopularityAndTime <- cbind(test_data$popularity,test_data$Year)
dfPopuAndTime <- data.frame(PopularityAndTime)
names(dfPopuAndTime) <- c("popularity","Year")
PopuAndTimeFit <- rpart(Year~.,data = dfPopuAndTime,method = "class")
summary(PopuAndTimeFit)
popularityNewData2 <- data.frame(popularity = dfPopuAndTime$popularity)
PopuAndTimePredicted <- predict(PopuAndTimeFit,newdata = popularityNewData2)
rpart.plot(PopuAndTimeFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)


##Release date and Revenue
RevenueAndTime <- cbind(test_data$revenue,test_data$Year)
dfRevenueAndTime <- data.frame(RevenueAndTime)
names(dfRevenueAndTime) <- c("revenue","Year")
RevenueAndTimeFit <- rpart(Year~.,data = dfRevenueAndTime,method = "class")
summary(RevenueAndTimeFit)
RevenueNewData2 <- data.frame(revenue = dfRevenueAndTime$revenue)
RevenueAndTimePredicted <- predict(RevenueAndTimeFit,newdata = RevenueNewData2)
rpart.plot(RevenueAndTimeFit,branch=1,shadow.col = "gray",box.col="green",border.col="blue",split.col="red",split.cex=1.2)

