library(tidyverse)
# UCB algorithm
source("UCB.R")

# N=1000
res_1k <- UCB(1000,probs=c(0.5,0.55,0.55,0.45))
plot(res_1k$chosen)
hist(res_1k$chosen)
ggplot(res_1k,aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick") 

# N=10000
res_10k <- UCB(10000,probs=c(0.5,0.55,0.55,0.45))
plot(res_10k$chosen)
hist(res_10k$chosen)
ggplot(res_10k,aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick") 
