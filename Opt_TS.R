# optimistic Thompson sampling

Opt_TS = function(N,probs, alpha, beta, seed){
  
  arms <- length(probs)
  set.seed(seed)
  # create empty dataframe
  df <- as.data.frame(matrix(ncol=arms, nrow=N))
  for (i in 1:arms) {
    df[,i] <- rbinom(N,1,probs[i]) 
    #define column names
    names(df[,i]) <- paste0("bern",i)
  }  
  # number of arms
  k = ncol(df)
  # number of instances
  t = nrow(df)
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
    hps[arm,1] = hps[arm,1] + df[i,arm]
    # beta
    hps[arm,2] = hps[arm,2] + 1 - df[i,arm]
    arms = c(arms, arm)
  }
  return(cbind(df,data.frame("chosen"=arms)))
  # return(hps)
}
