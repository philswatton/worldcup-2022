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
                     rating_home = rating),
            by = "home_team") %>%
  left_join(teams %>% 
              rename(away_team = team,
                     rating_away = rating),
            by = "away_team") %>%
  mutate(home_win = 1 / (1 + 10**(-(rating_home - rating_away)/400)))




# Monte Carlo Simulation ----

# seed and n
set.seed(3298)
N <- 25000#100000

# Number of groups
num_group <- 8

# Initialise results storage
results_group <- vector(mode='list', length=num_group)
results_16 <- NULL
results_quarter <- NULL
results_semi <- NULL
results_final <- vector(mode='character', length=N)

# Progress bar
pb = txtProgressBar(min=0,
                    max=N,
                    initial=0,
                    char="=",
                    style=3) 

# Loop
for (n in 1:N) {
  
  # Update progress
  setTxtProgressBar(pb,n)
  
  
  
  # Ignoring draws for simplicity
  group_probs$result <- NA
  for (i in 1:nrow(group_probs)) {
    group_probs$result[i] <- sample(c(0,1), size=1, prob=c(1 - group_probs$home_win[i], group_probs$home_win[i]))
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
  
  # Pick 1st and 2nd, deal with tie breakers (results can be 3,3,0,0; 3,1,1,1; 3,2,1,0; 2,2,1,1; or 2,2,2,0)
  for (i in 1:8) {
    # Two tie breaks
    if (all(group_ranks[[i]]$wins == c(3,3,0,0)) | all(group_ranks[[i]]$wins == c(2,2,1,1))) {
      group_ranks[[i]] <- group_ranks[[i]][c(sample(1:2),sample(3:4)),]
      if (n == 1) {
        results_group[[i]] <- t(group_ranks[[i]]$team)
      } else {
        results_group[[i]] <- rbind(results_group[[i]],group_ranks[[i]]$team)
      }
    }
    
    # Triple-break: 3111
    if (all(group_ranks[[i]]$wins == c(3,1,1,1))) {
      group_ranks[[i]] <- group_ranks[[i]][c(1,sample(2:4)),]
      if (n == 1) {
        results_group[[i]] <- t(group_ranks[[i]]$team)
      } else {
        results_group[[i]] <- rbind(results_group[[i]],group_ranks[[i]]$team)
      }
    }
    
    # Triple-break: 2220
    if (all(group_ranks[[i]]$wins == c(2,2,2,0))) {
      group_ranks[[i]] <- group_ranks[[i]][c(sample(1:3),4),]
      if (n == 1) {
        results_group[[i]] <- t(group_ranks[[i]]$team)
      } else {
        results_group[[i]] <- rbind(results_group[[i]],group_ranks[[i]]$team)
      }
    }
    
    # Nice order: 3210
    if (all(group_ranks[[i]]$wins == c(3,2,1,0))) {
      if (n == 1) {
        results_group[[i]] <- t(group_ranks[[i]]$team)
      } else {
        results_group[[i]] <- rbind(results_group[[i]],group_ranks[[i]]$team)
      }
    }
  }
  
  
  
  # Generate 16s fixtures
  # A1 vs B2
  # C1 vs D2
  # E1 vs F2
  # G1 vs H2
  # B1 vs A2
  # D1 vs C2
  # F1 vs E2
  # H1 vs G2
  fixtures_16 <- data.frame(
    home_team = c(group_ranks[[1]]$team[1], #A1
                  group_ranks[[3]]$team[1],
                  group_ranks[[5]]$team[1],
                  group_ranks[[7]]$team[1],
                  group_ranks[[2]]$team[1],
                  group_ranks[[4]]$team[1],
                  group_ranks[[6]]$team[1],
                  group_ranks[[8]]$team[1]),
    away_team = c(group_ranks[[2]]$team[2], #B2
                  group_ranks[[4]]$team[2],
                  group_ranks[[6]]$team[2],
                  group_ranks[[8]]$team[2],
                  group_ranks[[1]]$team[2],
                  group_ranks[[3]]$team[2],
                  group_ranks[[5]]$team[2],
                  group_ranks[[7]]$team[2])
  ) %>% left_join(teams %>% 
              rename(home_team = team,
                     rating_home = rating),
              by = "home_team") %>%
    left_join(teams %>% 
                rename(away_team = team,
                       rating_away = rating),
              by = "away_team") %>%
    mutate(home_win = 1 / (1 + 10**(-(rating_home - rating_away)/400)))
  
  # Simulate 16s results
  num_16 <- 8
  wins_16 <- rep(NA_real_, num_16)
  for (i in 1:8) {
    wins_16[i] <- sample(c(0,1), 1, prob=c(1 - fixtures_16$home_win[i], fixtures_16$home_win[i]))
  }
  
  # Vector of 16s results
  teams_16 <- vector(mode='character', length=num_16)
  for (i in 1:num_16) {
    teams_16[i] <- ifelse(wins_16[i] == 1, fixtures_16$home_team[i], fixtures_16$away_team[i])
  }
  
  # Store results
  if (n == 1) {
    results_16 <- t(teams_16)
  } else {
    results_16 <- rbind(results_16, teams_16)
  }
  
  
  
  # Generate quater finals fixtures
  # R16-1 vs R16-2
  # R16-3 vs R16-4
  # R16-5 vs R16-6
  # R16-7 vs R16-8
  fixtures_quarter <- data.frame(
    home_team = c(teams_16[seq(1,7,2)]),
    away_team = c(teams_16[seq(2,8,2)])
  ) %>% left_join(teams %>% 
                    rename(home_team = team,
                           rating_home = rating),
                  by = "home_team") %>%
    left_join(teams %>% 
                rename(away_team = team,
                       rating_away = rating),
              by = "away_team") %>%
    mutate(home_win = 1 / (1 + 10**(-(rating_home - rating_away)/400)))
  
  # Simulate quarter results
  num_quarter <- 4
  wins_quarter <- rep(NA_real_, num_quarter)
  for (i in 1:num_quarter) {
    wins_quarter[i] <- sample(c(0,1), 1, prob=c(1 - fixtures_quarter$home_win[i], fixtures_quarter$home_win[i]))
  }
  
  # Vector of 16s results
  teams_quarter <- vector(mode='character', length=num_quarter)
  for (i in 1:num_quarter) {
    teams_quarter[i] <- ifelse(wins_quarter[i] == 1, fixtures_quarter$home_team[i], fixtures_quarter$away_team[i])
  }
  
  # Store results
  if (n == 1) {
    results_quarter <- t(teams_quarter)
  } else {
    results_quarter <- rbind(results_quarter, teams_quarter)
  }
  
  
  
  # Generate semi finals fixtures
  # QF-1 vs QF-2
  # QF-3 vs Qf-4
  fixtures_semi <- data.frame(
    home_team = c(teams_quarter[c(1,3)]),
    away_team = c(teams_quarter[c(2,4)])
  ) %>% left_join(teams %>% 
                    rename(home_team = team,
                           rating_home = rating),
                  by = "home_team") %>%
    left_join(teams %>% 
                rename(away_team = team,
                       rating_away = rating),
              by = "away_team") %>%
    mutate(home_win = 1 / (1 + 10**(-(rating_home - rating_away)/400)))
  
  # Simulate semi results
  num_semi <- 2
  wins_semi <- rep(NA_real_, num_semi)
  for (i in 1:num_semi) {
    wins_semi[i] <- sample(c(0,1), 1, prob=c(1 - fixtures_semi$home_win[i], fixtures_semi$home_win[i]))
  }
  
  # Vector of 16s results
  teams_semi <- vector(mode='character', length=num_semi)
  for (i in 1:num_semi) {
    teams_semi[i] <- ifelse(wins_semi[i] == 1, fixtures_semi$home_team[i], fixtures_semi$away_team[i])
  }
  
  # Store results
  if (n == 1) {
    results_semi <- t(teams_semi)
  } else {
    results_semi <- rbind(results_semi, teams_semi)
  }
  
  
  
  # Generate finals fixtures
  # SF-1 vs SF-2
  fixtures_final <- data.frame(
    home_team = teams_semi[1],
    away_team = teams_semi[2]
  ) %>% left_join(teams %>% 
                    rename(home_team = team,
                           rating_home = rating),
                  by = "home_team") %>%
    left_join(teams %>% 
                rename(away_team = team,
                       rating_away = rating),
              by = "away_team") %>%
    mutate(home_win = 1 / (1 + 10**(-(rating_home - rating_away)/400)))
  
  # Simulate semi results
  wins_final <- sample(c(0,1), 1, prob=c(1 - fixtures_final$home_win, fixtures_final$home_win))
  
  # Vector of 16s results
  winner <- ifelse(wins_final == 1, fixtures_final$home_team, fixtures_final$away_team)
  
  # Store results
  results_final[n] <- winner
  
}



# Storing Results ----

save(group_probs, results_group, results_16, results_quarter, results_semi, results_final, file="results/results.RData")



