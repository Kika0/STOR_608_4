library(tidyverse)
# UCB algorithm
source("UCB.R")
# TS algorithm
source("TS.R")
source("KL_UCB.R")

# TS
ts <- TS(N=1000,probs=c(0.5,0.55),alpha=1,beta=1) %>% mutate("chosen"=as.character(chosen))
ggplot(ts,aes(x=chosen)) +
  geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal()

# UCB
ucb <- UCB(1000,probs=c(0.5,0.55)) %>% mutate("chosen"=as.character(chosen))
ggplot(ucb,aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal()

# KL-UCB
# UCB
kl_ucb <- KL_UCB(1000,probs=c(0.5,0.55)) %>% mutate("chosen"=as.character(chosen))
ggplot(ucb,aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal()

ts <- ts %>% rename("Thompson sampling"=chosen)
ucb <- ucb %>% rename("UCB"=chosen) 
kl_ucb <- ucb %>% rename("KL-UCB"=chosen)
all <- cbind(ts,ucb,kl_ucb) %>% pivot_longer(c(UCB,"Thompson sampling"),names_to = "algorithm",values_to = "chosen") %>% select(-c(arm,arm.1))

ggplot(all,aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal() +
  facet_wrap(~algorithm)
