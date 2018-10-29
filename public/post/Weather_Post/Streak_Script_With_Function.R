# library(devtools)
# devtools::install_github("statswithr/statsr")
library(dplyr)
library(ggplot2)

#Script which simulates a baseketball players shooting and then uses a function to
#calculate streaks. 

#Seattle gets a total of 


#create a vector of Hits and Misses, must be "H" or "M" for calc_streak function.
outcomes <- data.frame(c("H", "M"))
#create a simulation of 200 shots using dplyr's sample_n
Seattle_Weather <- sample_n(outcomes, 100000, replace = T, weight = c(.43, .57))



#look at table
table(Seattle_Weather)


#pass df to calc_streak()
streak <- calc_streak(Seattle_Weather)
#Look at table
table(streak)

ggplot(streak, aes(length)) + 
  geom_histogram() + 
  scale_x_continuous(breaks = seq(0,15,1)) +
  theme_bw()



Seattle_Weather$Weather[1+1]

x <- 1:10

for(i in 1:length(x)){
  print(i + 2)
}

#function
calc_streak = function(x){
  if (!is.atomic(x))
    x = x[,1]
  if (any(!x %in% c("H","M")))
    stop('Input should only contain hits ("H") and misses ("M")')
  y = rep(0,length(x))
  y[x == "H"] = 1
  y = c(0, y, 0)
  wz = which(y == 0)
  streak = diff(wz) - 1
  return(data.frame(length = streak))
}



