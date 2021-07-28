##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org") 

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(Metrics)
library(corrplot)
library(RColorBrewer)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##Data Cleaning and, data exploration and Data Visulization 
#In order to determine if age of the movie is a factor for predicting rating, I extracted the premiere date of the movie, and then calculated the age of the movie. I will also looked at individual genres for genre effect, as well as, effects of user ratings.
head(edx)
glimpse(edx)

#How many distinct movie, users and genres
n_distinct(edx$movieId)

n_distinct(edx$genres)

n_distinct(edx$userId)

nrow(edx)

#Convert Timestamp to year
edx_with_year <- mutate(edx, year_rated = year(as_datetime(timestamp)))
head(edx_with_year)

validation_with_year <- mutate(validation, year_rated = year(as_datetime(timestamp)))

#extracting the premiere date

edx_premiere <- stringi::stri_sub(edx_with_year$title,-5,-2)%>% as.numeric()
validation_premiere <- stringi::stri_sub(validation_with_year$title,-5,-2)%>% as.numeric()

edx_genres <- edx$genres
validation_genres <- validation$genres

#Add the premiere date
edx_with_title_dates <- edx_with_year %>% mutate(premiere_date = edx_premiere)

validation_with_title_dates <- validation_with_year %>% mutate(premiere_date = validation_premiere)

#drop the timestamp
edx_with_title_dates <- edx_with_title_dates %>% select(-timestamp)
validation_with_title_dates <- validation_with_title_dates %>% select(-timestamp)

head(edx_with_title_dates)

#Calculate the age of a movie and genre details
edx_with_genre_details <- edx_with_title_dates %>% mutate(age_of_movie = 2018 - premiere_date, 
                                                          rating_date_range = year_rated - premiere_date,
                                                          primary_genre = sub("\\|.*", "", genres),
                                                          genre_count = count.fields(textConnection(genres), sep = "|"))
head(edx_with_genre_details)

validation_with_genre_details <- validation_with_title_dates %>% mutate(age_of_movie = 2018 - premiere_date, 
                                                                        rating_date_range = year_rated - premiere_date,
                                                                        primary_genre = sub("\\|.*", "", genres),
                                                                        genre_count = count.fields(textConnection(genres), sep = "|"))

#Distribution of Movie Ratings
edx_with_genre_details %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "cadetblue3", color = "grey20", bins = 10) +
  scale_x_log10() +
  ggtitle("Number of Movies Ratings")

#Distribution of Users
edx_with_genre_details %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "cadetblue3", color = "grey20", bins = 10) +
  scale_x_log10() + 
  ggtitle("Number of Users Ratings")

#Movie rating averages
movie_avgs <- edx_with_genre_details %>% group_by(movieId) %>% summarize(avg_movie_rating = mean(rating))
user_avgs <- edx_with_genre_details %>% group_by(userId) %>% summarize(avg_user_rating = mean(rating))
year_avgs <- edx_with_genre_details %>% group_by(year_rated) %>% summarize(avg_rating_by_year = mean(rating)) #year the movie was rated
age_avgs <- edx_with_genre_details %>% group_by(age_of_movie) %>% summarize(avg_rating_by_age = mean(rating)) #age of movie
genrecount_avgs <- edx_with_genre_details %>% group_by(genre_count) %>% summarize(avg_rating_by_genre_count = mean(rating)) #genre count of movie

print(age_avgs)

print(genrecount_avgs)

# age of movie vs average movie rating
age_avgs %>%
  ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point() +
  ggtitle("Age of a Movie vs Average Movie Rating")

# userId vs average movie rating
user_avgs %>%
  ggplot(aes(userId, avg_user_rating)) +
  geom_point(alpha = 1/20, colour = "red") +
  ggtitle("User vs Average User Rating")

#Split the data into single genres
edx_split_genre <- edx_with_genre_details %>% separate_rows(genres, sep ="\\|")

head(edx_split_genre)

validation_split_genre <- validation_with_genre_details %>% separate_rows(genres, sep ="\\|")

genre_count_by_movieId <- edx_split_genre %>% group_by(movieId, genres) %>% summarize(n = n())

number_of_genres <- edx_split_genre %>% group_by(genres) %>% summarize(n = n())
number_of_genres

#Distribution of Ratings per Genre
temp <- edx_split_genre %>%
  group_by(genres) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n/sumN) %>%
  arrange(-percentage)


temp %>%
  ggplot(aes(reorder(genres, percentage), percentage, fill= percentage)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "YlOrRd") + labs(y = "Percentage", x = "Genre") +
  ggtitle("Distribution of Genres by Percent Rated")


temp <- edx_split_genre %>%
  group_by(genres) %>%
  summarize(mean_rating_by_genre=mean(rating)) %>%
  arrange(-mean_rating_by_genre)

temp %>%
  ggplot(aes(reorder(genres, mean_rating_by_genre), mean_rating_by_genre, fill= mean_rating_by_genre)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "YlOrRd") + labs(y = "Mean Rating", x = "Genre") +
  ggtitle("Average Rating of Genres")

#Is there a correlation

#Number of movie ratings per movie
n_movies_ratings <- edx_with_genre_details %>% group_by(movieId) %>% summarize(n = n())

#Average Movie Rating for each movie
avg_movie_rat <- edx_with_genre_details %>% group_by(movieId) %>% summarize(avg_m_r = mean(rating))

#Create correlation data
cor_dat <- edx_with_genre_details %>% select(rating, movieId, userId, year_rated, age_of_movie, rating_date_range, premiere_date) %>%
  left_join(n_movies_ratings, by = "movieId") %>%
  left_join(avg_movie_rat, by = 'movieId')
head(cor_dat)

#Is there a relationship between number of ratings and the average rating
get_cor <- function(df){
  m <- cor(df$x, df$y, use="pairwise.complete.obs");
  eq <- substitute(italic(r) == cor, list(cor = format(m, digits = 2)))
  as.character(as.expression(eq));
}

#Number of ratings vs avg movie ratings
cor_dat %>%
  ggplot(aes(n, avg_m_r)) + stat_bin_hex(bins = 50) + scale_fill_distiller(palette = "Spectral") +
  stat_smooth(method = "lm", color = "orchid", size = 1) +
  annotate("text", x = 20000, y = 2.5, label = get_cor(data.frame(x = cor_dat$n, y = cor_dat$avg_m_r)), 
           parse = TRUE, color = "orchid", size = 7) + ylab("Average Movie Rating") + xlab("Number of Ratings")


cor_dat %>% 
  ggplot(aes(age_of_movie, avg_m_r)) + stat_bin_hex(bins = 50) + scale_fill_distiller(palette = "Spectral") +
  stat_smooth(method = "lm", color = "orchid", size = 1) +
  annotate("text", x = 75, y = 0.9, label = get_cor(data.frame(x = corr_by_age_of_movie$age_of_movie, y = corr_by_age_of_movie$avg_m_r)), 
           parse = TRUE, color = "orchid", size = 7) + ylab("Average Movie Rating") + xlab('Age of Movie')


movie_avgs_norm <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs_norm %>% qplot(b_i, geom ="histogram", bins = 20, data = ., color = I("black"))

user_avgs_norm <- edx %>% 
  left_join(movie_avgs_norm, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs_norm %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))

# baseline Model: just the mean 
baseline_rmse <- RMSE(validation$rating,mu)
## Test results based on simple prediction
baseline_rmse

## Check results
rmse_results <- data_frame(method = "Using mean only", RMSE = baseline_rmse)
rmse_results

# Movie effects only 
predicted_ratings_movie_norm <- validation %>% 
  left_join(movie_avgs_norm, by='movieId') %>%
  mutate(pred = mu + b_i) 
model_1_rmse <- RMSE(validation$rating,predicted_ratings_movie_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))

# Use test set,join movie averages & user averages
# Prediction equals the mean with user effect b_u & movie effect b_i
predicted_ratings_user_norm <- validation %>% 
  left_join(movie_avgs_norm, by='movieId') %>%
  left_join(user_avgs_norm, by='userId') %>%
  mutate(pred = mu + b_i + b_u) 
# test and save rmse results 
model_2_rmse <- RMSE(validation$rating,predicted_ratings_user_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and User Effect Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)
# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time 
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(validation$rating,predicted_ratings))
})
# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

# Compute regularized estimates of b_i using lambda
movie_avgs_reg <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())
# Compute regularized estimates of b_u using lambda
user_avgs_reg <- edx %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())
# Predict ratings
predicted_ratings_reg <- validation %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% 
  .$pred
# Test and save results
model_3_rmse <- RMSE(validation$rating,predicted_ratings_reg)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie and User Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

# b_y and b_g represent the year & genre effects, respectively
lambdas <- seq(0, 20, 1)
# Note: the below code could take some time 
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx_split_genre %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_with_genre_details %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- edx_with_genre_details %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(premiere_date) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda), n_y = n())
  
  b_g <- edx_with_genre_details %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'premiere_date') %>%
    group_by(primary_genre) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda), n_g = n())
  predicted_ratings <- validation_with_genre_details %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'premiere_date') %>%
    left_join(b_g, by = 'primary_genre') %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
    .$pred
  
  return(RMSE(validation_with_genre_details$rating,predicted_ratings))
})
# Compute new predictions using the optimal lambda
# Test and save results 
qplot(lambdas, rmses)  
lambda_2 <- lambdas[which.min(rmses)]
lambda_2

movie_reg_avgs_2 <- edx_with_genre_details %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_2), n_i = n())
user_reg_avgs_2 <- edx_with_genre_details %>% 
  left_join(movie_reg_avgs_2, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_2), n_u = n())
year_reg_avgs <- edx_with_genre_details %>%
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  group_by(premiere_date) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_2), n_y = n())
genre_reg_avgs <- edx_with_genre_details %>%
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  left_join(year_reg_avgs, by = 'premiere_date') %>%
  group_by(primary_genre) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_2), n_g = n())
predicted_ratings <- validation_with_genre_details %>% 
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  left_join(year_reg_avgs, by = 'premiere_date') %>%
  left_join(genre_reg_avgs, by = 'primary_genre') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
  .$pred
model_4_rmse <- RMSE(validation_with_genre_details$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Reg Movie, User, Year, and Genre Effect Model",  
                                     RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()

#b_gc represents the genre count effect
lambdas <- seq(0, 20, 1)
# Note: the below code could take some time 
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx_split_genre %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_with_genre_details %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- edx_with_genre_details %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(premiere_date) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda), n_y = n())
  
  b_g <- edx_with_genre_details %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'premiere_date') %>%
    group_by(primary_genre) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda), n_g = n())
  
  b_gc <- edx_with_genre_details %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'premiere_date') %>%
    left_join(b_g, by = 'primary_genre') %>%
    group_by(genre_count) %>%
    summarize(b_gc = sum(rating - mu - b_i - b_u - b_y - b_g)/(n()+lambda), n_gc = n())
  
  predicted_ratings <- validation_with_genre_details %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'premiere_date') %>%
    left_join(b_g, by = 'primary_genre') %>%
    left_join(b_gc, by = 'genre_count') %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g + b_gc) %>% 
    .$pred
  
  return(RMSE(validation_with_genre_details$rating,predicted_ratings))
})
# Compute new predictions using the optimal lambda
# Test and save results 
qplot(lambdas, rmses)  

lambda_3 <- lambdas[which.min(rmses)]
lambda_3

movie_reg_avgs_3 <- edx_with_genre_details %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_3), n_i = n())
user_reg_avgs_3 <- edx_with_genre_details %>% 
  left_join(movie_reg_avgs_3, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_3), n_u = n())
year_reg_avgs_2 <- edx_with_genre_details %>%
  left_join(movie_reg_avgs_3, by='movieId') %>%
  left_join(user_reg_avgs_3, by='userId') %>%
  group_by(premiere_date) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_3), n_y = n())
genre_reg_avgs_2 <- edx_with_genre_details %>%
  left_join(movie_reg_avgs_3, by='movieId') %>%
  left_join(user_reg_avgs_3, by='userId') %>%
  left_join(year_reg_avgs_2, by = 'premiere_date') %>%
  group_by(primary_genre) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_3), n_g = n())
genre_count_reg_avgs <- edx_with_genre_details %>%
  left_join(movie_reg_avgs_3, by='movieId') %>%
  left_join(user_reg_avgs_3, by='userId') %>%
  left_join(year_reg_avgs_2, by = 'premiere_date') %>%
  left_join(genre_reg_avgs_2, by = 'primary_genre') %>%
  group_by(genre_count) %>%
  summarize(b_gc = sum(rating - mu - b_i - b_u - b_y - b_g)/(n()+lambda_3), n_gc = n())


predicted_ratings <- validation_with_genre_details %>% 
  left_join(movie_reg_avgs_3, by='movieId') %>%
  left_join(user_reg_avgs_3, by='userId') %>%
  left_join(year_reg_avgs_2, by = 'premiere_date') %>%
  left_join(genre_reg_avgs_2, by = 'primary_genre') %>%
  left_join(genre_count_reg_avgs, by = 'genre_count') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g + b_gc) %>% 
  .$pred
model_5_rmse <- RMSE(validation_with_genre_details$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Reg Movie, User, Year, Genre and Genre Count Effect Model",  
                                     RMSE = model_5_rmse ))
rmse_results %>% knitr::kable()



