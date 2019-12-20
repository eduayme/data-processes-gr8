# Load up ggplot2 package to use for visualizations and dplyr for data manipulation
#install.packages(ggplot2)
#install.packages(dplyr)
#install.packages(vcd)
#install.packages(tidyr)
#install.packages(tidyverse)
#install.packages(GGally)
#install.packages(FactoMineR)
#install.packages(factoextra)
#install.packages(gridExtra)
#install.packages(lattice)
#install.packages(caret)
#install.packages(rpart)

library(ggplot2)
library(dplyr)
library(vcd)
library(tidyr)
library(tidyverse) #data manipilation
library(GGally) # nice scatterplot matrix
library(FactoMineR) # PCA computation
library(factoextra) # nice plotting for PCA objects
library(gridExtra) # to build grid of plots
library(lattice) # to build more plot
library(caret) # to create traning and test dataset
library(rpart) # to create the tree

# Load data set
Sys.setlocate("LC_ALL","English") # to avoid dataset have some unknow error of system language
# titanic <- read.csv("Asignaturas/IDA/movies.RData", header = TRUE)
movies <- read.csv(file="./datasets/tmdb_5000_movies.csv", header=TRUE, sep=",")

# view(movies)
#quitamos donde no hay datos para trabajar mejor
movies2 <- na.omit(movies)
view(movies2)
movies<-movies2
#theres a lot of films with revenue=0 and busget=0, we donw want this
# Ask the same question of our data using the pipe operator
movies2 <- movies2 %>% # data frame to start with
  filter(budget>0) %>% # 1. Filter down to only 2008 votes
  filter(revenue>0) 

## see some characteristichs
dim(movies2)
names(movies2)
str(movies2)
summary(movies2)
summary(movies2$budget)
summary(movies2$genres)
summary(movies2$status)

#genres are all in same column, baad, we take only the first genre (the most important i hope)
Genre<-movies2$genres
Genre<-as.data.frame(Genre)
Genre<-separate(Genre, col = Genre, into=c("1","2","3","4","5","6"))
##View(Genre)
movies2$genres<-Genre$`5`
movies2 <- na.omit(movies2)
#we have removed an extra film with no genre

# factor as factor and the column release date as date (spanish format)
movies2$genres=as.factor(movies2$genres)
#movies2$original_languaje=as.factor(movies2$original_languaje)
movies2$production_countries=as.factor(movies2$production_countries)
#movies2$spoken_languajes=as.factor(movies2$spoken_languajes)
movies2$status=as.factor(movies2$status)

#movies2$release_date <- as.Date(movies2$release_date, format="%d/%m/%Y")

#graph relations between budget and revenue
plot1=ggplot(data=movies2, aes(x=budget, y=revenue))
plot1+geom_jitter(aes(x=budget, color=revenue), position=position_jitter(w=.3, h=.0))

#graph relations between budget and revenue with color as vote average
plot2=ggplot(data=movies2, aes(x=budget, y=revenue))
plot2+geom_jitter(aes(x=budget,y=revenue, color=vote_average), position=position_jitter(w=.3, h=.0))
#we dont see anything clear, 2 more graph with vote average

#graph relations between vote average and revenue
plot3=ggplot(data=movies2, aes(x=vote_average, y=revenue))
plot3+geom_jitter(aes(x=vote_average, color=revenue), position=position_jitter(w=.3, h=.0))

#graph relations between budget and vote average
plot4=ggplot(data=movies2, aes(x=vote_average, y=budget))
plot4+geom_jitter(aes(x=vote_average, color=budget), position=position_jitter(w=.3, h=.0))

##with genre, is genre related with vote average and score??
#graph relations between genre and vote average
gen_vot <- ggplot(movies2, aes(x=genres, y=revenue),las=2) + 
  geom_violin()
gen_vot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#we can se thar all are similar, but in drama there are a lot more of negsatives films, and in history all have good votes

#there are a lot of films who doesnt have many votes (half of movies have less than 471 votes), we have to use only fimls which have more than 178 votes
#(1st quartile)
movies_reduced_votes <- movies2 %>% # data frame to start with
  filter(vote_count>178) 

#repeat same graph
#graph relations between genre and revenue
gen_vot <- ggplot(movies_reduced_votes, aes(x=genres, y=revenue)) + 
  geom_violin()
gen_vot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#total different graph, action, drama and science fiction earns a lot more money

#making a facet with vote integers
movies_reduced_votes$vote_average = sapply(movies_reduced_votes$vote_average, function(x) floor(x/1))
movies_reduced_votes$vote_average<-as.factor(movies_reduced_votes$vote_average)

#repeat same graph
#graph relations between genre and revenue
gen_vot <- ggplot(movies_reduced_votes, aes(x=genres, y=revenue)) + 
  geom_violin(aes(fill=vote_average))
gen_vot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#total different graph, action, drama and science fiction earns a lot more money

gen_vot<-gen_vot+facet_wrap(~vote_average)
gen_vot+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
###seems like we have a lot of genres who deoesnt have much revenue, we will inspect the films of the principal genres(action,adventure,animation,comedy,
#drama,family,fantasy,romance,science-fiction, thriller)


#### correlation between popularity and score or number of 
movies_reduced_votes2 <- movies_reduced_votes %>% # data frame to start with
  filter(genres=="Action" | genres=="Adventure" | genres=="Animation" | genres=="Comedy" | genres=="Drama" | genres=="Family" | genres=="Fantasy" | genres=="Romance" | genres=="Thriller" | genres=="Science")
#we only lost like 400 movies, they were not common films then, good
gen_vot <- ggplot(movies_reduced_votes2, aes(x=genres, y=revenue)) + 
  geom_violin()
gen_vot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

gen_vot<-gen_vot+facet_wrap(~vote_average)
gen_vot+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
## we can see that action films earn much more money than the rest when the vote_average rounds the 7. Drama and science ficiton behind

#lets see revenue vs budget in action films compared to vote score, only for action movies
action_movies <- movies_reduced_votes2 %>%
  filter(genres=="Action")

#graph relations between the budget and revenue
plotAction=ggplot(data=movies2, aes(x=budget, y=revenue))
plotAction+geom_jitter(aes(x=budget, color=revenue), position=position_jitter(w=.3, h=.0))

gen_vot <- ggplot(action_movies, aes(x=budget, y=revenue)) + 
  geom_violin()
gen_vot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
gen_vot<-gen_vot+facet_wrap(~vote_average)
gen_vot+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
# wow, las que tienen un 8 ni gastan mucho ni ganan poco, y seguimos comprobando que ganan mucho las que tienen un budget medio






####################### compare revenue with year
movies2$release_date <- as.Date(movies$release_date, "%Y-%m-%d")
head(movies2)

p <- ggplot(movies2, aes(x=release_date, y=revenue)) +
  geom_point(color="steelblue") +
  xlab("")
p

movies3 <- movies2 %>%
  mutate( year = format(release_date, "%Y")) %>%
  group_by( year) %>%
  mutate(revenue_mean=mean(revenue)) %>%
  mutate(budget_mean=mean(budget)) 
# summarise(total = mean(revenue))

p <- ggplot(movies3, aes(x=release_date, y=revenue_mean)) +
  geom_point() +
  geom_line(color="steelblue")+
  xlab("")
p

p <- ggplot(movies3, aes(x=release_date, y=budget_mean)) +
  geom_point() +
  geom_line(color="steelblue")+
  xlab("")
p



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


