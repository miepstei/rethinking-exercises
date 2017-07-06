library(rethinking)

# Easy

p_grid <- seq( from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(1000)
samples <- sample(x = p_grid, replace = TRUE, size = 1e4, prob = posterior)

#3E1 How much posterior probability lies below p = 0.2
sum(posterior[p_grid < 0.2])

#3E2 How much posterior probability lies above p = 0.8
sum(posterior[p_grid > 0.8])

#3E3 How much posterior probability lies between p = 0.2 and p = 0.8
sum(posterior[p_grid >= 0.2 & p_grid <= 0.8])

#3E4 20% of the posterior lies below which value of p?
quantile(samples, 0.2)

#3E4 20% of the posterior lies above which value of p?
quantile(samples, 0.8)

#3E5 narrowest interval containing 66% posterior probability
HPDI(samples, prob=0.66)

#3E5 narrowest interval containing 66% posterior probability equal either side
PI(samples, 0.66)

#medium
#3M1 - change data to 8 waters in 15 tosses
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

#3M2 - draw 10,000 samples
set.seed(1000)
samples <- sample(x = p_grid, replace = TRUE, size = 1e4, prob = posterior)
HPDI(samples, prob = 0.9)

#3M3 - posterior predictive check
new.data <- rbinom(1e4, size = 15, prob = samples)
hist(new.data)

#3M4 -P(8 water in 15 tosses)
length(new.data[new.data == 8])/1e4

#3M5 -P(6 water in 9 tosses)
new.data2 <- rbinom(1e4, size = 9, prob = samples)
length(new.data[new.data == 6])/1e4

#3M6 - new prior - 0 below 0.5
prior <- c(rep(0, 500), rep(1, 500))
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(1000)
samples <- sample(x = p_grid, replace = TRUE, size = 1e4, prob = posterior)
new.data <- rbinom(1e4, size = 9, prob = samples)

#90% credible interval for the posterior.
HPDI(samples, prob = 0.9)


