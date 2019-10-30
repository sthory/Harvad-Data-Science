"Exercise 2 - Recalculating the SIDS Statistics
Let's assume that there is in fact a genetic component to SIDS 
and the the probability of Pr(second case of SIDS???first case of 
SIDS)=1/100, is much higher than 1 in 8,500.

What is the probability of both of Sally Clark's sons dying of SIDS?

Instructions
Calculate the probability of both sons dying to SIDS."

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. 
# Print this value to the console.
Pr_1*Pr_2

# [1] 1.176471e-06

# 4-------------------------------------------------------------
"Exercise 4 - Calculate the Probability
Assume that the probability of a murderer finding a way to kill 
her two children without leaving evidence of physical harm is:

Pr(two children found dead with no evidence of harm???mother is a 
murderer)=0.50
Assume that the murder rate among mothers is 1 in 1,000,000.

Pr(mother is a murderer)=1/1,000,000
According to Bayes' rule, what is the probability of:

Pr(mother is a murderer???two children found dead with no evidence 
of harm)
Instructions
Use Bayes rule to calculate the probability that the mother is a 
murderer, considering the rates of murdering mothers in the population, 
the probability that two siblings die of SIDS, and the probability 
that a murderer kills children without leaving evidence of physical harm."

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB <- (Pr_BA * Pr_A) / Pr_B
Pr_AB

# [1] 0.425

# 6------------------------------------------------------------
"Exercise 6 - Back to Election Polls
Florida is one of the most closely watched states in the U.S. 
election because it has many electoral votes and the election 
is generally close. Create a table with the poll spread results 
from Florida taken during the last days before the election using 
the sample code.

The CLT tells us that the average of these spreads is approximately 
normal. Calculate a spread average and provide an estimate of 
the standard error.

Instructions

Calculate the average of the spreads. Call this average avg in the 
final table.
Calculate an estimate of the standard error of the spreads. Call 
this standard error se in the final table.
Use the mean and sd functions nested within summarize to find the 
average and standard deviation of the grouped spread data.
Save your results in an object called results."

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarize(avg = mean(spread), se = sd(spread)/sqrt(n()))
results

#           avg          se
# 1 0.004154545     0.007218692

# 7--------------------------------------------------------
"Exercise 7 - The Prior Distribution
Assume a Bayesian model sets the prior distribution for 
Florida's election night spread d to be normal with expected 
value ?? and standard deviation ??.

What are the interpretations of ?? and ???"

# ?? and ?? summarize what we would predict for Florida before seeing any polls.[X]

# 8 -------------------------------------------------------
"Exercise 8 - Estimate the Posterior Distribution
The CLT tells us that our estimate of the spread d^ has a 
normal distribution with expected value d and standard deviation 
??, which we calculated in a previous exercise.

Use the formulas for the posterior distribution to calculate the 
expected value of the posterior distribution if we set ??=0 and ??=0.01.

Instructions
100 XP
Define ?? and ??
Identify which elements stored in the object results represent ?? and Y
Estimate B using ?? and ??
Estimate the posterior distribution using B, ??, and Y"

# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <- (sigma^2) / (sigma^2 + tau^2)

# Calculate the expected value of the posterior distribution
(B*mu) + ((1 - B) * Y)

# [1] 0.002731286

# 9-----------------------------------------------------------
"Exercise 9 - Standard Error of the Posterior Distribution
Compute the standard error of the posterior distribution.

Instructions

Using the variables we have defined so far, calculate the standard
error of the posterior distribution.
Print this value to the console."

# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1 / ((1/sigma^2) + (1/tau^2)))

# [1] 0.005853024

# 10-----------------------------------------------------------
"Exercise 10- Constructing a Credible Interval
Using the fact that the posterior distribution is normal, create 
an interval that has a 95% of occurring centered at the posterior 
expected value. Note that we call these credible intervals.

Instructions

Calculate the 95% credible intervals using the qnorm function.
Save the lower and upper confidence intervals as an object called 
ci. Save the lower confidence interval first."

# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
E <- B*mu + (1-B)*Y

ci <- c(E - qnorm(0.975)*se, E + qnorm(0.975)*se)
ci

# [1] -0.008740432  0.014203003

#11--------------------------------------------------------
"Exercise 11 - Odds of Winning Florida
According to this analysis, what was the probability that 
Trump wins Florida?

Instructions

Using the pnorm function, calculate the probability that the 
spread in Florida was less than 0."

# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0,exp_value,se)

# [1] 0.3203769

# 12----------------------------------------------------------
"Exercise 12 - Change the Priors
We had set the prior variance ?? to 0.01, reflecting that these 
races are often close.
Change the prior variance to include values ranging from 0.005 
to 0.05 and observe how the probability of Trump winning Florida 
changes by making a plot.

Instructions

Create a vector of values of taus by executing the sample code.
Create a function using function(){} called p_calc that takes 
the value tau as the only argument, then calculates B from tau 
and sigma, and then calculates the probability of Trump winning, 
as we did in the previous exercise.
Apply your p_calc function across all the new values of taus.
Use the plot function to plot ?? on the x-axis and the new 
probabilities on the y-axis.
"

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau){
  B <- sigma^2 / (sigma^2 + tau^2)
  exp_value <- B*mu + (1-B)*Y 
  se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
  pnorm(0,exp_value,se)
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)



