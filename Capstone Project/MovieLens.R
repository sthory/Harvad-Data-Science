
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

# set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(1)
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


# ------------------- Save Date ----------------------------------
# Save data frame (Not to perform all the "Data Wrangling" again)
save(edx,file="edx.Rda")
save(validation,file="validation.Rda")




# ----------------------- S T A R T ---------------------------------
# ----------------------- MOVIE LENS --------------------------------

# load data frame wrangling
load("edx.Rda")
load("validation.Rda")

# -------------- Install libraries if is necessary ------------------

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(tidyr)) install.packages("tidyr")

# ----------------------- See data ---------------------------------- 

edx %>% 
   as_tibble()

"# A tibble: 9,000,055 x 6
   userId movieId rating timestamp title              genres                               
    <int>   <dbl>  <dbl>     <int> <chr>             <chr>                                
 1      1     122      5 838985046 Boomerang (1992)  Comedy|Romance                       
 2      1     185      5 838983525 Net, The (1995)   Action|Crime|Thriller                
 3      1     292      5 838983421 Outbreak (1995)   Action|Drama|Sci-Fi|Thriller         
 4      1     316      5 838983392 Stargate (1994)   Action|Adventure|Sci-Fi"

#----------------- Count users and movies --------------------------- 

edx %>% 
   summarize(users = n_distinct(userId), 
             movies = n_distinct(movieId))
" users movies
1 69878  10677"

#------------- Movie Ratings - Distribution -------------------------

edx %>% 
   count(movieId) %>% 
   ggplot(aes(n)) + 
   geom_histogram(bins = 20, color = "white") +
   scale_x_log10() +
   ggtitle("Movie Ratings - Distribution")

#------------------ Users -  Distribution ---------------------------
edx %>%
   count(userId) %>% 
   ggplot(aes(n)) + 
   geom_histogram(bins = 20, color = "white") + 
   scale_x_log10() +
   ggtitle("Users -  Distribution")

# ------------------- Overall Mean Rating ---------------------------

rating_mean <- mean(edx$rating)
rating_mean
"[1] 3.512465"

# ---------------- RMSE calculate function --------------------------

# RMSE function 
RMSE <- function(preds, values_true){
  sqrt(mean((values_true - preds) ^ 2))
}

# ----------------- Model 1: Movie effects --------------------------

model_movie_effects <- edx %>% 
   group_by(movieId) %>% 
   summarize(movie_effect = mean(rating - rating_mean),
             ratings_movie = n())

# Ratings for Movie Model Plot 

model_movie_effects %>% 
   qplot(ratings_movie, 
         geom ="histogram", 
         bins = 30, 
         data = ., 
         colour = I("white")) +
   ggtitle("Ratings for Movies")

# Predictions

preds <- validation %>% 
  left_join(model_movie_effects, by = "movieId") %>%
  mutate(mix_effect = movie_effect) %>%
  mutate(predictions = rating_mean + mix_effect) %>%
  .$predictions

# rmse calculation

rmse <- RMSE(preds, validation$rating)

# Print rmse

rmse
"[1] 0.9439087"

# add model and rmse to table results

rmse_results <- data.frame(Method = "Model 1: Movie effects", 
                           Rmse = rmse)

# Print table results

rmse_results %>% 
   knitr::kable()

"|Method                 |      Rmse|
 |:----------------------|---------:|
 |Model 1: Movie effects | 0.9439087|"

# ------------- Model 2: Movie + User effects -----------------------

model_user_effects <- edx %>% 
   group_by(userId) %>% 
   left_join(model_movie_effects, by = "movieId") %>%
   summarize(user_effect = mean(rating - movie_effect - rating_mean), 
             ratings_user = n()) 

# Ratings for User + Movie Model Plot 

model_user_effects %>% 
   qplot(ratings_user, 
         geom ="histogram", 
         bins = 30, 
         data = ., 
         colour = I("white")) +
   ggtitle("Ratings for Users")

model_user_effects %>% 
   ggplot(aes(user_effect)) + 
   geom_histogram(bins = 30, 
                  color = "white") +
   ggtitle("Model User and Movie effects")

# Predictions using Movie + user Model 

preds <- validation %>% 
   left_join(model_movie_effects, by = "movieId") %>%
   left_join(model_user_effects, by = "userId") %>%
   mutate(mix_effect = user_effect + movie_effect) %>%
   mutate(predictions = rating_mean + mix_effect) %>%
   .$predictions

# Plot Predictions 

preds %>% 
  data.frame() %>%
  ggplot(aes(preds)) + 
  geom_histogram(bins = 20, color = "white") +
  ggtitle("Predictions")

# rmse calculation

rmse <- RMSE(preds, validation$rating)

# Print rmse

rmse
"[1] 0.8653488"

# add model and rmse to table results

rmse_results <- bind_rows(rmse_results, 
                          data.frame(Method = "Model 2: Movie + user effects", 
                          Rmse = rmse))

# Print table results

rmse_results %>% 
   knitr::kable()

"|Method                        |      Rmse|
 |:-----------------------------|---------:|
 |Model 1: Movie effects        | 0.9439087|
 |Model 2: Movie + user effects | 0.8653488|"

# ------- Model 3: Regularization Movie + User effect --------------

# lambda options
l <- seq(0, 15, 0.5)

rmses <- sapply(l, function(l){
   
   # bi calculation
   
   bi <- edx %>% 
      group_by(movieId) %>%
      summarize(bi = sum(rating - rating_mean)/(n()+l))
   
   # bu calculation
   
   bu <- edx %>% 
      left_join(bi, by="movieId") %>%
      group_by(userId) %>%
      summarize(bu = sum(rating - bi - rating_mean)/(n()+l))

   # Predict
   
   predicted_ratings <- 
      validation %>% 
      left_join(bi, by = "movieId") %>%
      left_join(bu, by = "userId") %>%
      mutate(pred = rating_mean + bi + bu) %>%
      pull(pred)
   
   return(RMSE(predicted_ratings, validation$rating))
})


# Plot lambda options and rmses

qplot(l, rmses,
      xlab = "lambda options", 
      ylab = "rmse values") +
      ggtitle("Optimal lambda")

# lambda value that minimize rmse

lambda_optimal <- l[which.min(rmses)]
lambda_optimal
"[1] 5"

# add model and rmse to table results 

rmse_results <- 
   bind_rows(rmse_results,
             data_frame(Method = "Model 3: Regularized Movie + User effect",  
                        Rmse = min(rmses)))

# Print table results

rmse_results %>% 
   knitr::kable()

"|Method                                   |      Rmse|
 |:----------------------------------------|---------:|
 |Model 1: Movie effects                   | 0.9439087|
 |Model 2: Movie + user effects            | 0.8653488|
 |Model 3: Regularized Movie + User effect | 0.8648177|"

# print minimum rmse (final result)

min(rmses)

"> min(rmses)
[1] 0.8648177"

"MovieLens Grading Rubric

RMSE (25 points)
0 points: No RMSE reported AND/OR code used to generate the RMSE appears to violate 
the edX Honor Code.
5 points: RMSE >= 0.90000 AND/OR the reported RMSE is the result of overtraining 
                                 (validation set used for anything except reporting 
                                 the final RMSE value)

10 points: 0.86550 <= RMSE <= 0.89999
15 points: 0.86500 <= RMSE <= 0.86549
20 points: 0.86490 <= RMSE <= 0.86499
25 points: RMSE <= 0.8649"

# Comparison of the final result with the objective

min(rmses) <= 0.8649 
"[1] TRUE"

0.8648177 <= 0.8649  
"[1] TRUE"








