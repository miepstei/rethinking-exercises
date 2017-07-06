library(rethinking)

data(homeworkch3)

pB <- sum(c(birth1, birth2))/(length(birth1) + length(birth2))

p_grid <- seq( from = 0, to = 1, length.out = 1000)
prior <- rep(x = 1, 1000)
likelihood <- dbinom(x = sum(c(birth1, birth2)), size = length(c(birth1, birth2)), prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior /sum(posterior)

#max P
p_grid[which.max(posterior)]

#sample 10,000 samples from the posterior
posterior.samples <- sample(x = p_grid, size = 1e4, prob = posterior, replace = TRUE)

sapply(X = c(0.5, 0.89, 0.97), function(x) HPDI(posterior.samples, prob = x))

#create some new data
new.data <- rbinom(n = 1e4, size = 200, prob = posterior.samples)
dens(new.data)

#new data just from first borns
likelihood <- dbinom(x = sum(birth1), size = length(birth1), prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior /sum(posterior)
posterior.samples2 <- sample(x = p_grid, size = 1e4, prob = posterior, replace = TRUE)
new.data2 <- rbinom(n = 1e4, size = 200, prob = posterior.samples2)
dens(new.data2)

#model assumes first births are independent of second. Retrieve second male births where
# first birth is female
cond.first.female <- birth2[birth1 == 0]
likelihood <- dbinom(x = sum(cond.first.female), size = length(cond.first.female), prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior /sum(posterior)
posterior.samples3 <- sample(x = p_grid, size = 1e4, prob = posterior, replace = TRUE)
new.data3 <- rbinom(n = 1e4, size = length(cond.first.female), prob = posterior.samples3)
dens(new.data3, adj = 1)


