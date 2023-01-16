# Giro (garbage in ,reward out algorithm)

Giro = function(N,probs,seed){
  
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
  # dataframe of pseudo rewards
  his = data.frame(matrix(ncol = 2, nrow = 0))
  # colnames(his) <- c("arm1","arm2")
  # recording the chosen arms
  arms = c()
  # carrying out Giro
  for(i in 1:t){
    mus = c()
    # generating beta sample for each arm
    for(j in 1:k){
      if (length(na.omit(his[,j]))> 0) {
      numerator = sample(x=na.omit(his[,j]),size=length(na.omit(his[,j])),replace = TRUE)
      denominator = length(na.omit(his[,j]))
      
      mu = sum(numerator)/denominator
      mus = cbind(mus, mu)
      } else {
        mu=10^6
        mus = cbind(mus, mu)
      }
    }
    # picking the arm with greatest value
    arm = as.numeric(sample(as.character(which(mus == max(mus))),1))
    # generate pseudo rewards to attach to pseudo history of the chosen arm
    # note that c(-1,1) are pseudo rewards
    pseudo_rewards <- c(0,1)
    rewards <- data.frame(matrix(ncol = 2, nrow = 3))
    rewards_i= c(df[i,arm],pseudo_rewards)
    rewards[,arm] <- rewards_i
    his= rbind(his,rewards)
    arms = c(arms, arm)
  }
  return(cbind(df,data.frame("chosen"=arms)))
  # return(hps)
}