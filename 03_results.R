# GV2022 World Cup Predictions
# Phil Swatton
# University of Essex
# File 03: Results


# Packages
library(tidyverse)
library(combinat)


# Data
load("data/team_ratings.RData")
load("data/fixtures.RData")
load("results/results.RData")


# Set up df to store %s in
teams <- teams %>%
  left_join(fixtures %>%
              select(team=home_team, group) %>%
              filter(!duplicated(.)),
            by="team")

# Sim Length
N <- 25000


# Group Rank Orders ----

# with teams a, b, c, d, there are 24 possible rank orders
# use combinat::permn(x), where x is the vector of teams

map(results_group, function(x) {
  orders <- permn(unique(x[1,])) %>% reduce(rbind)
  order_count <- vector(mode="double", length=24)
  for (i in 1:N) {
    index <- which(orders[,1] == x[i,1] & orders[,2] == x[i,2] & orders[,3] == x[i,3] & orders[,4] == x[i,4])
    order_count[index] <- order_count[index] + 1
    
  }
  return(orders[which.max(order_count),])
})



# Make it to 16 ----
made16 <- map(results_group, function(x) {
  return(x[,1:2])
}) %>%
  reduce(rbind) %>%
  table()
sort(made16/N, decreasing=T)




# 16s Winners ----

(table(results_16)/N*100) %>% sort(decreasing=T)



# Quarters Winners ----

(table(results_quarter)/N*100) %>% sort(decreasing=T)



# Semi Winners ----

(table(results_semi)/N*100) %>% sort(decreasing=T)



# Final Winners ----

(table(results_final)/N*100) %>% sort(decreasing=T)


