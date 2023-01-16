library(tidyverse)
# UCB algorithm
source("UCB.R")
# TS algorithm
source("TS.R")
# optimistic TS algorithm
source("Opt_TS.R")
# KL-UCB algorithm
source("KL_UCB.R")
# source("giro1.R")
# Giro algorithm
source("Giro.R")

# UCB
ucb <- UCB(1000,probs=c(0.5,0.55)) %>% mutate("chosen"=as.character(chosen))
ggplot(ucb,aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal()

# KL-UCB
# UCB
kl_ucb <- KL_UCB(1000,probs=c(0.5,0.55)) %>% mutate("chosen"=as.character(chosen))
ggplot(kl_ucb,aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal()

# TS
ts <- TS(N=1000,probs=c(0.5,0.55),alpha=1,beta=1) %>% mutate("chosen"=as.character(chosen))
ggplot(ts,aes(x=chosen)) +
  geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal()

# Optimistic TS
opt_ts <- Opt_TS(N=1000,probs=c(0.5,0.55),alpha=1,beta=1) %>% mutate("chosen"=as.character(chosen))
ggplot(opt_ts,aes(x=chosen)) +
  geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal()

# Giro
giro <- Giro(N=1000,probs=c(0.5,0.55)) %>% mutate("chosen"=as.character(chosen))
ggplot(giro,aes(x=chosen)) +
  geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal()

# bring together to one plot
ucb <- ucb %>% rename("UCB"=chosen) 
kl_ucb <- kl_ucb %>% rename("KL-UCB"=chosen)
ts <- ts %>% rename("Thompson sampling"=chosen)
opt_ts <- opt_ts %>% rename("Optimistic Thompson sampling"=chosen)
giro <- giro %>% rename("Giro"=chosen)
all <- cbind(ucb,kl_ucb,ts,opt_ts,giro) %>% pivot_longer(c(UCB,"KL-UCB","Thompson sampling","Optimistic Thompson sampling","Giro"),names_to = "algorithm",values_to = "chosen") %>% select(-c(arm,arm.1)) 
ggplot(all %>% mutate(algorithm=factor(algorithm,level=c("UCB","KL-UCB","Thompson sampling","Optimistic Thompson sampling","Giro"))),aes(x=chosen)) + geom_histogram(binwidth = 0.5,fill="firebrick",stat="count") +
  xlab("") +
  ylab("Count") +
  theme_minimal() +
  facet_wrap(~algorithm)
