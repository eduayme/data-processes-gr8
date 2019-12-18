setwd("D:/AplicacionesInformaticas/workspaceR")

# Load up ggplot2 package to use for visualizations and dplyr for data manipulation
library(ggplot2)
library(dplyr)
library("vcd")
library(tidyr)

library(tidyverse) #data manipilation
library(GGally) # nice scatterplot matrix
library(FactoMineR) # PCA computation
library(factoextra) # nice plotting for PCA objects
library(gridExtra) # to build grid of plots

# Load data set
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

##with genre, is genre related with vote average and score?¿
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

#graph relations between busgete and revenue
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
