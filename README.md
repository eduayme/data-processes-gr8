# Movies
##Domain of interest
People are generally interested in movies, so we have thought that is an interest domain to choose for doing this assignment. We also want to explore different relationships between the attributes of distinct films.

We have found some data science projects related to this domain. Some of them are:

- [Data science analysis of movies released in the cinema between 2000 and 2017](https://medium.com/datadriveninvestor/data-science-analysis-of-movies-released-in-the-cinema-between-2000-and-2017-b2d9e515d032 "Data science analysis of movies released in the cinema between 2000 and 2017")
- [The most influential factor of imdb movie rating part in data scraping](https://towardsdatascience.com/the-most-influential-factor-of-imdb-movie-rating-part-i-data-scraping-61dc0c4dd518 "The most influential factor of imdb movie rating part in data scraping")
-  [What makes a successful film predicting a films revenue and user rating with machine learning?](https://towardsdatascience.com/what-makes-a-successful-film-predicting-a-films-revenue-and-user-rating-with-machine-learning-e2d1b42365e7 "What makes a successful film predicting a films revenue and user rating with machine learning?")
- [Analyzing Movie Scores on IMDb and Rotten Tomatoes](http://rstudio-pubs-static.s3.amazonaws.com/336722_2193716117584b63a2a6ebb837217d85.html "Analyzing Movie Scores on IMDb and Rotten Tomatoes")

Some example of questions that we will try to find are:
- What attributes makes the film better (with better we refer to the money earned, the views, the ratings)?
- Does the duration of film influence the critic score?
- Are some film genres more popular than others? Does it change from one year to another?

##Finding Data
We have downloaded the data from the following websites:
- [TMDB Movie Metadata](https://www.kaggle.com/tmdb/tmdb-movie-metadata#tmdb_5000_movies.csv "TMDB Movie Database")
 - Collected by Kaggle. Extracted by getting the best 5000 films.
 -  4807 rows, 20 columns.
- [IMDB Data](https://www.kaggle.com/PromptCloudHQ/imdb-data "IMDB Data") 
 - Collected by Kaggle. Extracted by querying the REST Api of IMDB to get the best 1000 films.
 - 1000 rows, 12 columns.
- [Movies RData (extracted from IDA course, may not be accesible)](https://moodle.upm.es/titulaciones/oficiales/pluginfile.php/1535921/mod_resource/content/3/_site/movies.html)
 - We do not know.
 - 651 rows, 32 columns.

All the questions that we have propose can be answered with any of the datasets that we have choosen.
