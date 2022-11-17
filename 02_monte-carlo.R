# GV2022 World Cup Predictions
# Phil Swatton
# University of Essex
# File 02: Monte Carlo


# Packages
library(tidyverse)


# Data
load("data/team_ratings.RData")
load("data/fixtures.RData")



# Calculate Group Fixture Probabilities ----
group_probs <- fixtures %>%
  left_join(teams %>% 
              rename(home_team = team,
                     rating_home = rating)) %>%
  left_join(teams %>% 
              rename(away_team = team,
                     rating_away = rating)) %>%
  mutate(home_win = 1 / (1 + 10**(-(rating_home - rating_away)/400)))




# Monte Carlo Simulation ----

# seed and n
set.seed(3298)
N <- 10

# Loop
for (n in 1:N) {
  
  # Ignoring draws for simplicity
  group_probs$result <- NA
  for (i in 1:nrow(group_probs)) {
    group_probs$result[i] <- sample(c(0,1), size=1, prob=c(group_probs$home_win[i], 1 - group_probs$home_win[i]))
  }
  
  # Get group results
  group_dfs <- group_split(group_probs, group)
  group_ranks <- map(group_dfs, function(x) {
    
    group_rank <- data.frame(
      team = unique(x$home_team),
      wins = 0
    )
    
    for (j in 1:6) {
      if (x$result[j] == 1) {
        group_rank$wins[group_rank$team == x$home_team[j]] = group_rank$wins[group_rank$team == x$home_team[j]] + 1
      } else {
        group_rank$wins[group_rank$team == x$away_team[j]] = group_rank$wins[group_rank$team == x$away_team[j]] + 1
      }
    }
    
    group_rank <- group_rank %>%
      arrange(-wins)
    
    return(group_rank)
    
  })
  
  # Pick 1st and 2nd, deal with tie breakers (results can be 3,2,1,0;  2,2,1,1; or 2,2,2,0)

  
}






