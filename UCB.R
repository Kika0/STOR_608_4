# creating the UCB Algorithm for Bernoulli

UCB = function(N,probs){
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
      mu = numerator/denominator + sqrt(2*log(i)/denominator)
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