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


rw <- function(n = 1000, repetitions = 3){
  
  if(repetitions == 0){
    print("why do you want to plot 0 random walks?")
  }
  else{
    repetitions <- round(repetitions)
    if(repetitions < 0){
      print("Please enter a positive integer")
    }
    else{
  
      # this function creates a number of random walks of length n
      
      tseries = NULL
    
      # let's create a random serie
      for(i in 1:repetitions){
        x <- e <- rnorm(n)
        
        # generate random walks
        for(t in 2:n){
          x[t] <- x[t - 1] + e[t]
        }
        
        x <- as.data.frame(x)
        
        tseries <- bind_cols(tseries, x)
        
      }
      
      # name the series
      colnames(tseries) <- sprintf("S%s", seq(1:repetitions))
      
      # create a time variable
      tseries$t <- 1:n
      
      # create the plot background
      
      if(repetitions > 1){
        p <- p <- ggplot(data = tseries,
                         aes(x = t)) +
          theme_classic() +
          theme(
            
            panel.grid.major.y = element_line(color = "black",
                                              linetype = "dotted",
                                              size = 0.1),
            panel.grid.minor.y = element_line(color = "black",
                                              linetype = "dotted"),
            plot.title = element_text(hjust = 0.5)
          ) +
          xlab("Time") +
          ylab("Value at time t") +
          labs(title = paste(as.character(repetitions),
                             "random walks generated"))
      
      }
      else{
        p <- p <- ggplot(data = tseries,
                         aes(x = t)) +
          theme_classic() +
          theme(
            
            panel.grid.major.y = element_line(color = "black",
                                              linetype = "dotted",
                                              size = 0.1),
            panel.grid.minor.y = element_line(color = "black",
                                              linetype = "dotted"),
            plot.title = element_text(hjust = 0.5)
          ) +
          xlab("Time") +
          ylab("Value at time t") +
          labs(title = paste(as.character(repetitions),
                             "random walk generated"))    
      }
    
      # add the layers of each random walk
      
      for(i in names(tseries)[-length(tseries)]){
        p <- p +  geom_line(aes_string(y = i,
                                       color = shQuote(rgb(viridis.map[runif(1,1,nrow(viridis.map)),1:3]))),
                            show.legend = F)
      }
      
      return(p)
    }
  }
}