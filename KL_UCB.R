# define KL divergence for Bernoulli by truncating
eps <- 1e-15
KL <- function(p,q) {
  y <- c()
  out <- c()
  for (i in 1:length(q)) {
  x = min(max(p, eps), 1 - eps)
  y[i] = min(max(q[i], eps), 1 - eps)
  out[i] <-  x * log(x/y[i]) + (1-x)*log((1-x)/(1-y[i]))
  }
  return(out)
}
# test KL function
# q <- seq(0.7,1,0.01)
# kl <- rep(NA,length(q))
# for (i in 1:length(q)) {
#   kl[i] <- KL(0.7,q[i])
# }

KL_UCB = function(N,probs){
  # if (arms!=length(probs)) {print("Probabilities vector must have same length as the number of arms.")}
  arms <- length(probs)
  set.seed(1)
  # create empty dataframe
  df <- as.data.frame(matrix(ncol=arms, nrow=N))
  for (i in 1:arms) {
    df[,i] <- rbinom(N,1,probs[i]) 
    #define column names
    names(df[,i]) <- paste0("bern",i)
  }  
  
  # total no iterations
  t = nrow(df)
  # number of arms
  k = ncol(df)
  # observed df
  arms = NULL
  for(m in (1:k)){
    arms = cbind(arms,arm = df[1:k,m])
  }
  # initial chosen arms
  chosen = seq(1,k)
  # add a column for chosen arms
  obs <<- cbind(arms, chosen)
  obs <<- data.frame(obs)
  # ucb for remaining time steps
  for(i in (k+1):t){
    mus = c()
    for(j in 1:k){
      numerator = sum(obs[which(obs[,k+1] == j),j])
      denominator = sum(as.numeric(obs[,k+1] == j))
      q <- seq(numerator/denominator,1,0.01)
      
      q <- q[numerator*KL(p=numerator/denominator,q=q)<=log(i)]
      mu = max(q)
      # mu = numerator/denominator + sqrt(2*log(i)/denominator)
      mus = cbind(mus, mu)
    }
    arm = which.max(mus)
    if (length(arm) != 1){
      arm = sample(arm, 1)
    }
    new = cbind(df[i,], arm)
    colnames(new) = colnames(obs)
    obs <<- rbind(obs, new)
  }
  return(obs)
}
