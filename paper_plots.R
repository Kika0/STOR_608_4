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
giro <- Giro(N=1000,probs=c(0.5,0.55),seed=1) %>% mutate("chosen"=as.character(chosen))
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

# 1000 macroreplications of each algorithm
reg_giro <- c() 
for (i in 1:1000) {
  giro <- Giro(N=1000,probs=c(0.5,0.55),seed=i)
  reg_giro[i] <- 550-sum(giro$V1[giro$chosen==1]) - sum(giro$V2[giro$chosen==2])
}

reg_ucb <- c() 
for (i in 1:1000) {
  ucb <- UCB(N=1000,probs=c(0.5,0.55),seed=i)
  colnames(ucb) <- c("V1","V2","chosen")
  reg_ucb[i] <- 550-sum(ucb$V1[ucb$chosen==1]) - sum(ucb$V2[ucb$chosen==2])
}

reg_kl_ucb <- c() 
for (i in 1:1000) {
  kl_ucb <- KL_UCB(N=1000,probs=c(0.5,0.55),seed=i)
  colnames(kl_ucb) <- c("V1","V2","chosen")
  reg_kl_ucb[i] <- 550-sum(kl_ucb$V1[kl_ucb$chosen==1]) - sum(kl_ucb$V2[kl_ucb$chosen==2])
}

reg_ts <- c() 
for (i in 1:1000) {
  ts <- TS(N=1000,probs=c(0.5,0.55),alpha=1,beta=1,seed=i)
  reg_ts[i] <- 550-sum(ts$V1[ts$chosen==1]) - sum(ts$V2[ts$chosen==2])
}

reg_opt_ts <- c() 
for (i in 1:1000) {
  opt_ts <- Opt_TS(N=1000,probs=c(0.5,0.55),alpha=1,beta=1,seed=i)
  reg_opt_ts[i] <- 550-sum(opt_ts$V1[opt_ts$chosen==1]) - sum(opt_ts$V2[opt_ts$chosen==2])
}

rucb <- reg_ucb %>% as.data.frame()  
rkl_ucb <- reg_kl_ucb %>% as.data.frame() 
rts <- reg_ts %>% as.data.frame() 
ropt_ts <- reg_opt_ts %>% as.data.frame() 
rgiro <- reg_giro %>% as.data.frame() 
all <- cbind(rucb,rkl_ucb,rts,ropt_ts,rgiro)
colnames(all) <- c("UCB","KL-UCB","TS","Optimistic TS","Giro")
all <- all %>% pivot_longer(c(UCB,"KL-UCB","TS","Optimistic TS","Giro"),names_to = "algorithm",values_to = "chosen") 
ggplot(all %>% mutate(algorithm=factor(algorithm,level=c("UCB","KL-UCB","TS","Optimistic TS","Giro"))),aes(x=algorithm,y=chosen)) + geom_boxplot(binwidth = 0.5,fill="firebrick") +
  xlab("") +
  ylab("Regret for T=1000") +
  theme_minimal() 
  # facet_wrap(~algorithm)


