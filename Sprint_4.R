library(invgamma)

# question a

# data for wok iin
wi = c(rep(10,45), rep(8,8), rep(6,6), 4, rep(2,8))
df = data.frame(data = wi)
write.csv(df, "wokiin.csv")
plot(density(wi))

mean = mean(wi)
var = var(wi)

mu = mean
alpha = 100
beta = 1
lambda = (alpha-1)/(var*beta)

# normal inverse gamma

rnormgamma <- function(n, mu, lambda, alpha, beta) {
  if (length(n) > 1) 
    n <- length(n)
  sigma2 <- rinvgamma(n, alpha, beta)
  x <- rnorm(n, mu, sqrt(sigma2/lambda))
  data.frame(x = x)
}

plot(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))

### updating for posterior

data = c(9,5,6)

munew = (lambda*mu + sum(data))/(lambda + length(data))
lambdanew = lambda + length(data)
alphanew = alpha + length(data)/2
betanew = beta + 0.5*sum((data - mean(data))^2) + length(data)*lambda/(lambda + length(data))*(mean(data) - mu)^2/2

plot(density(rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x))
lines(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))

# sampling 1,000,000 datapoints from NIG rv

sample = rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x
length(sample[sample>8])/length(sample)
# data for etnas

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

plot(density(e))

mu = mean(e)
alpha = 100
beta = 1
lambda = (alpha-1)/(var(e)*beta)

plot(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))

### posterior updating

data = c(7,8)

munew = (lambda*mu + sum(data))/(lambda + length(data))
lambdanew = lambda + length(data)
alphanew = alpha + length(data)/2
betanew = beta + 0.5*sum((data - mean(data))^2) + length(data)*lambda/(lambda + length(data))*(mean(data) - mu)^2/2

plot(density(rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x))
lines(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))

# sampling 1,000,000 datapoints from NIG rv

sample = rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x
length(sample[sample>8])/length(sample)

# Sultan's reviews
reviews <- c(rep(10,554),rep(8,138),rep(6,67),rep(4,34),rep(2,52))
mu <- mean(reviews)
sig_2 <- var(reviews)
length(reviews)

mu = mean(reviews)
alpha = 100
beta = 1
lambda = (alpha-1)/(var(reviews)*beta)

plot(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))
plot(density(reviews))


### posterior updating

data = c(3,6,5)

munew = (lambda*mu + sum(data))/(lambda + length(data))
lambdanew = lambda + length(data)
alphanew = alpha + length(data)/2
betanew = beta + 0.5*sum((data - mean(data))^2) + length(data)*lambda/(lambda + length(data))*(mean(data) - mu)^2/2

plot(density(rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x))
lines(density(rnormgamma(10000, mu, lambda, alpha, beta)$x))

## probability question

# sampling 1,000,000 datapoints from NIG rv

sample = rnormgamma(10000, munew, lambdanew, alphanew, betanew)$x
length(sample[sample>8])/length(sample)

# question b

# loading in libraries
library(dplyr)

# creating the 1000x2 dataset

bern1 = rbinom(1000,1,0.5)
bern2 = rbinom(1000,1,0.55)

df1 = data.frame(bern1, bern2)

# 10000x2 dataset

bern3 = rbinom(10000,1,0.5)
bern4 = rbinom(10000,1,0.55)

df2 = data.frame(bern3, bern4)

# testing for multi-arm bandit problem

bern5 = rbinom(10000,1,0.5)
bern6 = rbinom(10000,1,0.55)
bern7 = rbinom(10000,1,0.6)
bern8 = rbinom(10000,1,0.65)

df3 = data.frame(bern5, bern6, bern7, bern8)

# far apart modes

bern1 = rbinom(1000,1,0.05)
bern2 = rbinom(1000,1,0.95)

df4 = data.frame(bern1, bern2)

# creating the UCB Algorithm

UCB = function(data){
  # total no iterations
  t = nrow(data)
  # number of arms
  k = ncol(data)
  # observed data
  arms = NULL
  for(m in (1:k)){
    arms = cbind(arms,arm = data[1:k,m])
  }
  # initial chosen arms
  chosen = seq(1,k)
  # add a column for chosen arms
  obs <<- cbind(arms, chosen)
  obs <<- data.frame(obs)
  print(obs)
  # ucb for remaining time steps
  for(i in (k+1):t){
    mus = c()
    for(j in 1:k){
      numerator = sum(obs[which(obs[,k+1] == j),j])
      denominator = sum(as.numeric(obs[,k+1] == j))
      mu = numerator/denominator + sqrt(2*log(i)/denominator)
      mus = cbind(mus, mu)
    }
    arm = which.max(mus)
    if (length(arm) != 1){
      arm = sample(arm, 1)
    }
    new = cbind(data[i,], arm)
    colnames(new) = colnames(obs)
    obs <<- rbind(obs, new)
  }
  return(obs)
}

res = UCB(df1)
plot(res$chosen)

UCB(df3)
res = UCB(df1)
res
hist(res$chosen)

# calculating regret

## do this later
res = UCB(df1)
vec = rep(0, nrow(res))
for (i in 1:length(vec)){
  if(res$chosen[i] != 2){
    vec[i] = 1
  }
}
vec = cumsum(vec)
plot(vec, type = "l")

# thompson sampling

TS = function(data, alpha, beta){
  # number of arms
  k = ncol(data)
  # number of instances
  t = nrow(data)
  # dataframe of hyperparameters
  hps = data.frame(A = rep(alpha,k), B = rep(beta,k))
  # recording the chosen arms
  arms = c()
  # carrying out TS
  for(i in 1:t){
    # creating a list for samples
    samples = c()
    # generating beta sample for each arm
    for(j in 1:k){
      samples[j] = rbeta(1, hps[j,1], hps[j,2])
    }
    # picking the arm with greatest value
    arm = which.max(samples)
    # update hyperparameter for that arm
    # alpha
    hps[arm,1] = hps[arm,1] + data[i,arm]
    # beta
    hps[arm,2] = hps[arm,2] + 1 - data[i,arm]
    arms = c(arms, arm)
  }
  # return(arms)
  return(hps)
}

hps

res = TS(df1,1,1)
hist(res)

# TS with 4 arms

res = TS(df3, 1,1)
hist(res)

# optimistic thompson sampling

OTS = function(data, alpha, beta){
  # number of arms
  k = ncol(data)
  # number of instances
  t = nrow(data)
  # dataframe of hyperparameters
  hps = data.frame(A = rep(alpha,k), B = rep(beta,k))
  # recording the chosen arms
  arms = c()
  # carrying out TS
  for(i in 1:t){
    # creating a list for samples
    samples = c()
    # generating beta sample for each arm
    for(j in 1:k){
      samples[j] = rbeta(1, hps[j,1], hps[j,2])
      if (samples[j] < hps[j,1]/(hps[j,1] + hps[j,2])){
        samples[j] = hps[j,1]/(hps[j,1] + hps[j,2])
      }
    }
    # picking the arm with greatest value
    arm = which.max(samples)
    # update hyperparameter for that arm
    # alpha
    hps[arm,1] = hps[arm,1] + data[i,arm]
    # beta
    hps[arm,2] = hps[arm,2] + 1 - data[i,arm]
    arms = c(arms, arm)
  }
  return(arms)
}


res = OTS(df1,1,1)
hist(res)
