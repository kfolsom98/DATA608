library(dplyr)
library(googleVis)

# Global values for Problem 1 #

df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv")

df <- df %>% filter(Year == 2010)



