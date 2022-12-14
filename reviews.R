library(tidyverse)
library(stargazer)

# data for wok iin
wi = c(rep(10,45), rep(8,8), rep(6,6), 4, rep(2,8))

# Sultan's reviews
reviews <- c(rep(10,554),rep(8,138),rep(6,67),rep(4,34),rep(2,52))
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
df <- tibble(Etna=table(e),"Wok Iin"=table(wi),"Sultan's"=table(reviews)) %>% mutate(Rank=c("2","4","6","8","10")) 
df <- df %>%   column_to_rownames("Rank")


stargazer(df,summary = F)
