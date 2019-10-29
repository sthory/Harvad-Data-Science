"Exercise 1 - Heights Revisited
We have been using urn models to motivate the use of probability models. However, most data 
science applications are not related to data obtained from urns. More common are data that 
come from individuals. Probability plays a role because the data come from a random sample. 
The random sample is taken from a population and the urn serves as an analogy for the population.
Let's revisit the heights dataset. For now, consider x to be the heights of all males in the data set. Mathematically speaking, x is our population. Using the urn analogy, we have an urn with the values of x in it.

What are the population average and standard deviation of our population?
Instructions
Execute the lines of code that create a vector x that contains heights for all males in the 
population.
Calculate the average of x.
Calculate the standard deviation of x."
library(tidyverse)

# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)


# Calculate the population standard deviation. Print this value to the console.
sd(x)

# 2-----------------------------------------------------------------------------------------
" Exercise 2 - Sample the population of heights
Call the population average computed above ?? and the standard deviation ??. Now take a sample 
of size 50, with replacement, and construct an estimate for ?? and ??.

Instructions
Use the sample function to sample N values from x.
Calculate the mean of the sampled heights.
Calculate the standard deviation of the sampled heights."

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = T)

# Calculate the sample average. Print this value to the console.
print(mean(X))

# Calculate the sample standard deviation. Print this value to the console.
print(sd(X))

# > print(mean(X))
# [1] 68.73423
 
# print(sd(X))
# [1] 3.761344

# --------------------------------------------------------------------------------
"Exercise 4 - Confidence Interval Calculation
We will use X? as our estimate of the heights in the population from our sample size N. 
We know from previous exercises that the standard estimate of our error X?????? is ??/N?????????.

Construct a 95% confidence interval for ??.

Instructions
Use the sd and sqrt functions to define the standard error se
Calculate the 95% confidence intervals using the qnorm function. Save the lower then the 
upper confidence interval to a variable called ci."

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console
X_hat <- mean(X)
se_hat <- sd(X)
se <- se_hat/sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(qnorm(0.025, X_hat, se), qnorm(0.975, X_hat, se))
ci

# se
# [1] 0.5319343

# > ci
# [1] 67.69166 69.77681

# 5 --------------------------------------------------------------------------------------
"Exercise 5 - Monte Carlo Simulation for Heights
Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals as you have 
just done. What proportion of these intervals include ???

Instructions
Use the replicate function to replicate the sample code for B <- 10000 simulations. Save the 
results of the replicated code to a variable called res. The replicated code should complete 
the following steps: -1. Use the sample function to sample N values from x. Save the sampled heights as a vector called X. -2. Create an object called interval that 
contains the 95% confidence interval for each of the samples. Use the same formula you used 
in the previous exercise to calculate this interval. -3. Use the between function to determine 
if ?? is contained within the confidence interval of that simulation.
Finally, use the mean function to determine the proportion of results in res that contain mu."

# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu

res <- replicate(B, {
  X <- sample(x, N, replace = T)
  X_hat <- mean(X)
  se_hat <- sd(X)
  se <- se_hat/sqrt(N)
  interval <- c(qnorm(0.025, X_hat, se),qnorm(0.975, X_hat, se))
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

# mean(res)
# [1] 0.9433

# 6----------------------------------------------------------------------------------------
"Exercise 6 - Visualizing Polling Bias
In this section, we used visualization to motivate the presence of pollster bias in election 
polls. Here we will examine that bias more rigorously. Lets consider two pollsters that 
conducted daily polls and look at national polls for the month before the election.

Is there a poll bias? Make a plot of the spreads for each poll.

Instructions
Use ggplot to plot the spread for each of the two pollsters.
Define the x- and y-axes usingusing aes() within the ggplot function.
Use geom_boxplot to make a boxplot of the data.
Use geom_point to add data points to the plot."

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research",
                         "The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>%
  ggplot(aes(pollster, spread)) +
  geom_boxplot() +
  geom_point()

# 7------------------------------------------------------------------------------------------
"Exercise 13 - Compute the Estimates
The answer to the previous question depends on ??1 and ??2, which we don't know. 
We learned that we can estimate these values using the sample standard deviation.

Compute the estimates of ??1 and ??2.

Instructions
Group the data by pollster.
Summarize the standard deviation of the spreads for each of the two pollsters. 
Name the standard deviation s.
Store the pollster names and standard deviations of the spreads (??) in an object 
called sigma."

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>% 
  group_by(pollster) %>% 
  summarize(s = sd(spread))

# Print the contents of sigma to the console
sigma
 
# 15-------------------------------------------------------------------------------------
"Exercise 15 - Calculate the 95% Confidence Interval of the Spreads
We have constructed a random variable that has expected value b2???b1, the pollster 
bias difference. If our model holds, then this random variable has an approximately 
normal distribution. The standard error of this random variable depends on ??1 and ??2, 
but we can use the sample standard deviations we computed earlier. We have everything 
we need to answer our initial question: is b2???b1 different from 0?

Construct a 95% confidence interval for the difference b2 and b1. Does this interval 
contain zero?

Instructions
Use pipes %>% to pass the data polls on to functions that will group by pollster and 
summarize the average spread, standard deviation, and number of polls per pollster.
Calculate the estimate by subtracting the average spreads. Save this estimate to a variable 
called estimate.
Calculate the standard error using the standard deviations of the spreads and the 
sample size. Save this value to a variable called se_hat.
Calculate the 95% confidence intervals using the qnorm function. Save the lower and then 
the upper confidence interval to a variable called ci.
"
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% 
  group_by(pollster) %>% 
  summarize(avg = mean(spread), 
            s = sd(spread), 
            N=n())
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.

estimate <- max(res$avg) - min(res$avg)
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.

se_hat <- sqrt(res$s[2]^2/res$N[2] +
                 res$s[1]^2/res$N[1])  
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate - qnorm(0.975)*se_hat ,estimate + qnorm(0.975)*se_hat)
ci

# res
# A tibble: 2 x 4
# pollster                                      avg      s     N
# <fct>                                       <dbl>  <dbl> <int>
#  1 Rasmussen Reports/Pulse Opinion Research 0.000625 0.0177    16
# 2 The Times-Picayune/Lucid                 0.0529   0.0268    24

# > estimate
# [1] 0.05229167

# > se_hat
# [1] 0.007031433

# > ci
# [1] 0.03851031 0.06607302

# 16-------------------------------------------------------------------------------
"Exercise 16 - Calculate the P-value
The confidence interval tells us there is relatively strong pollster effect resulting 
in a difference of about 5%. Random variability does not seem to explain it.

Compute a p-value to relay the fact that chance does not explain the observed pollster 
effect.

Instructions

Use the pnorm function to calculate the probability that a random value is larger than 
the observed ratio of the estimate to the standard error.
Multiply the probability by 2, because this is the two-tailed test."

# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
2* (1 - pnorm(estimate / se_hat, 0, 1))

# Probar 1 - (pnorm(estimate/se_hat) - (pnorm(-estimate/se_hat))) tambien funciona

# [1] 1.030287e-13

# 17---------------------------------------------------------------------------
"Exercise 17 - Comparing Within-Poll and Between-Poll Variability
We compute statistic called the t-statistic by dividing our estimate of b2???b1 by its 
estimated standard error:

Y?2???Y?1s22/N2+s21/N1???????????????????????????????????????
Later we learn will learn of another approximation for the distribution of this statistic
for values of N2 and N1 that aren't large enough for the CLT.

Note that our data has more than two pollsters. We can also test for pollster effect using 
all pollsters, not just two. The idea is to compare the variability across polls to variability 
within polls. We can construct statistics to test for effects and approximate their distribution. 
The area of statistics that does this is called Analysis of Variance or ANOVA. We do not cover 
it here, but ANOVA provides a very useful set of tools to answer questions such as: is there a 
pollster effect?

Compute the average and standard deviation for each pollster and examine the variability across 
the averages and how it compares to the variability within the pollsters, summarized by the 
standard deviation."

# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <- polls %>%
  group_by(pollster) %>%
  summarize(avg = mean(spread), s = sd(spread)) 

var

# A tibble: 11 x 3
# pollster                                       avg       s
#<fct>                                        <dbl>   <dbl>
#  1 ABC News/Washington Post                  0.0373   0.0339 
#  2 CVOTER International                      0.0279   0.0180 
#  3 Google Consumer Surveys                   0.0303   0.0185 
#  4 Gravis Marketing                          0.0160   0.0152 
#  5 IBD/TIPP                                  0.00105  0.0168 
#  6 Ipsos                                     0.0553   0.0195 
#  7 Morning Consult                           0.0414   0.0146 
#  8 Rasmussen Reports/Pulse Opinion Research  0.000625 0.0177 
#  9 The Times-Picayune/Lucid                  0.0529   0.0268 
# 10 USC Dornsife/LA Times                    -0.0213   0.0207 
# 11 YouGov                                    0.0398   0.00709
   
