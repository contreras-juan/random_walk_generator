# ---- libraries ----
library(ggplot2)
library(tidyverse)
library(dplyr)

# ---- A first random walk simulation ----

# set simulation seed
set.seed(12345)

# set lenght of the series
N = 1000

# create a random serie
x <- e <- rnorm(n = N)

# generate random walk
for (t in 2:N){
  x[t] <- x[t-1] + e[t]
}

tserie <- data.frame(x,
                     t = 1:1000)

# plot random walk
ggplot(data = tserie,
       aes(x = t,
           y = x)) +
  geom_line()

# ---- function for several random walks ----

# set lenght of the series
N = 1000
repetitions = 3
tseries <- NULL
# create a random serie

for(i in 1:repetitions){
  
  x <- e <- rnorm(n = N)
  # generate random walk
  for (t in 2:N){
    x[t] <- x[t-1] + e[t]
  }
  
  
  x <- as.data.frame(x)
  
  tseries <- bind_cols(tseries, x)
}
colnames(tseries) <- sprintf("S%s", seq(1:repetitions))
tseries$t <- 1:N

ggplot(data = tseries,
       aes(x = t)) +
  geom_line(aes(y = S1)) +
  geom_line(aes(y = S2)) +
  geom_line(aes(y = S3))