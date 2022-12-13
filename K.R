# Sultan's reviews
reviews <- c(rep(10,554),rep(8,138),rep(6,67),rep(4,34),rep(2,52))
mu <- mean(reviews)
sig_2 <- var(reviews)
length(reviews)

t <- 2 + (mu^2/sig_2)
f <- mu*(t-1)

fit = MASS::fitdistr(1/reviews, "gamma")
rev <- data.frame(reviews)
# install.packages("dirichletprocess")
library(dirichletprocess)
dirichletprocess::Fit(rev,its=10)
plot(density(reviews))
