library(tidyverse)
library(stargazer)
library(invgamma)

### create Tripadvisor reviews datasets ----
# data for wok iin
wi = c(rep(10,45), rep(8,8), rep(6,6), 4, rep(2,8))

# Sultan's reviews
reviews <- c(rep(10,554),rep(8,138),rep(6,67),rep(4,34),rep(2,52))

# Etna's reviews
e <- c()
for (i in 1:188){
  e <- c(e, 10)
}
for (i in 1:61){
  e <- c(e, 8)
}
for (i in 1:18){
  e <- c(e, 6)
}
for (i in 1:22){
  e <- c(e, 4)
}
for (i in 1:25){
  e <- c(e, 2)
}

df <- tibble(Etna=table(e),"Wok Iin"=table(wi),"Sultan's"=table(reviews)) %>% mutate(Rank=c("2","4","6","8","10")) 
df <- df %>%   column_to_rownames("Rank")


stargazer(df,summary = F)

# calculate mean from Trip advisor
avrg <- c(mean(e),mean(wi),mean(reviews))
av <- mean(avrg)


# question A ----

# normal inverse gamma generator

rnormgamma <- function(n, mu, lambda, alpha, beta) {
  if (length(n) > 1) 
    n <- length(n)
  sigma2 <- rinvgamma(n, alpha, beta)
  x <- rnorm(n, mu, sqrt(sigma2/lambda))
  data.frame(x = x)
}

# Wok Iin
mean = mean(wi)
var = var(wi)

mu = mean
alpha = 100
beta = 1
lambda = (alpha-1)/(var*beta)
# plot(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))

### updating for posterior

data = c(9,5,6)

munew = (lambda*mu + sum(data))/(lambda + length(data))
lambdanew = lambda + length(data)
alphanew = alpha + length(data)/2
betanew = beta + 0.5*sum((data - mean(data))^2) + length(data)*lambda/(lambda + length(data))*(mean(data) - mu)^2/2

par(mfrow = c(3,1))

plot(density(rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x), col = "blue", main = "Wok Iin Distribution", xlab = "Scores",
     xlim = c(7.8, 8.7), ylim = c(0,17))
lines(density(rnormgamma(10000, mu, lambda, alpha, beta)$x), col = "red")
abline(v = av, col = "black", lty = 2)
# sampling 1,000,000 datapoints from NIG rv

sample = rnormgamma(100000, munew, lambdanew, alphanew, betanew)$x
length(sample[sample>8])/length(sample)

# plot(density(e))

mu = mean(e)
# alpha = 100
# beta = 1
lambda = (alpha-1)/(var(e)*beta)

# plot(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))

### posterior updating

data = c(7,8)

munew = (lambda*mu + sum(data))/(lambda + length(data))
lambdanew = lambda + length(data)
alphanew = alpha + length(data)/2
betanew = beta + 0.5*sum((data - mean(data))^2) + length(data)*lambda/(lambda + length(data))*(mean(data) - mu)^2/2

plot(density(rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x), col = "blue", main = "Etna's Distribution", xlab = "Scores",
     xlim = c(7.8, 8.7), ylim = c(0,17))
lines(density(rnormgamma(10000, mu, lambda, alpha, beta)$x), col = "red")
abline(v = av, col = "black", lty = 2)

# sampling 1,000,000 datapoints from NIG rv

sample = rnormgamma(100000, munew, lambdanew, alphanew, betanew)$x
length(sample[sample>8])/length(sample)

# Sultan's
mu = mean(reviews)
# alpha = 100
# beta = 1
lambda = (alpha-1)/(var(reviews)*beta)
# 
# plot(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))
# plot(density(reviews))


### posterior updating

data = c(3,6,5)

munew = (lambda*mu + sum(data))/(lambda + length(data))
lambdanew = lambda + length(data)
alphanew = alpha + length(data)/2
betanew = beta + 0.5*sum((data - mean(data))^2) + length(data)*lambda/(lambda + length(data))*(mean(data) - mu)^2/2

plot(density(rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x), col = "blue", main = "Sultan's Distribution", xlab = "Scores",
     xlim = c(7.8, 8.7), ylim = c(0,17))
lines(density(rnormgamma(10000, mu, lambda, alpha, beta)$x), col = "red")
abline(v = av, col = "black", lty = 2)

## probability question

# sampling 1,000,000 datapoints from NIG rv

sample = rnormgamma(100000, munew, lambdanew, alphanew, betanew)$x
length(sample[sample>av])/length(sample)

