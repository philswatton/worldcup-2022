# GV2022 World Cup Predictions
# Phil Swatton
# University of Essex
# File 01: Elo Rankings


# Packages
library(tidyverse)
library(lubridate)


# Data (see README for links)
match_data <- read_csv("data/results.csv") %>%
  filter(complete.cases(.)) %>%
  mutate(home_team = case_when(home_team == "United States" ~ "USA",
                               T ~ home_team),
         away_team = case_when(away_team == "United States" ~ "USA",
                               T ~ away_team))
fixtures <- read_csv("data/fifa-world-cup-2022-GMTStandardTime.csv") %>%
  rename(home_team = `Home Team`,
         away_team = `Away Team`) %>%
  mutate(home_team = case_when(home_team == "Korea Republic" ~ "South Korea",
                               T ~ home_team),
         away_team = case_when(away_team == "Korea Republic" ~ "South Korea",
                               T ~ away_team)) %>%
  filter(!is.na(Group)) %>%
  select(home_team, away_team, group=Group)



# Match scores
matches <- match_data %>%
  mutate(result = case_when(home_score > away_score ~ 1,
                            home_score == away_score ~ 0.5,
                            T ~ 0),
         goal_diff = abs(home_score - away_score),
         # multiplier = case_when(goal_diff == 2 ~ 1.5,
         #                        goal_diff == 3 ~ 1.75,
         #                        goal_diff >= 4 ~ 1.75 + (goal_diff-3)/8,
         #                       T ~ 1)
         multiplier = case_when(goal_diff == 0 ~ 1,
                                T ~ sqrt(goal_diff))) %>%
  select(date, home_team, away_team, tournament, result, multiplier)


# Add importance weights to matches
# Somewhat based on:
# - http://eloratings.net/about for values
# - https://www.kaggle.com/code/lekroll/predictions-for-the-fifa-world-cup-2018-using-r/notebook for use of regex
# matches <- matches %>%
#   mutate(importance = case_when(str_detect(tournament,"FIFA") ~ 60,
#                                 str_detect(tournament, "UEFA") ~ 50,
#                                 str_detect(tournament, "Copa América") | str_detect(tournament, "African Cup of Nations") ~ 40,
#                                 !str_detect(tournament, "Friendly") ~ 30,
#                                 str_detect(tournament, "Friendly") ~ 20),
#          importance = case_when(str_detect(tournament, "qualification") ~ importance * 0.75,
#                                 T ~ importance),
#          importance = importance * multiplier)

# Helper
# helper <- read_csv("data/competition_index.csv") %>%
#   rename(importance = match_importance)
matches <- matches %>%
  # left_join(helper) %>%
  # filter(importance != "drop") %>%
  mutate(importance = 30)#as.numeric(importance)*multiplier)



# Elo Start ----
team_ratings <- data.frame(
  team = unique(c(matches$home_team, matches$away_team)),
  rating = 1500
)



# Elo Algo ----

# Loop over matches
for (i in 1:nrow(matches)) {
  
  # Teams
  home_team <- matches$home_team[i]
  away_team <- matches$away_team[i]
  
  # Current ratings
  rating_home <- team_ratings$rating[team_ratings$team == home_team]
  rating_away <- team_ratings$rating[team_ratings$team == away_team]
  
  # Compute expected result for both teams
  expected_home = 1 / (1 + 10**(-(rating_home - rating_away + 100)/400))
  expected_away = 1 - expected_home
  
  # Update ratings
  team_ratings$rating[team_ratings$team == home_team] <- rating_home + matches$importance[i] * (matches$result[i] - expected_home)
  team_ratings$rating[team_ratings$team == away_team] <- rating_away + matches$importance[i] * (1 - matches$result[i] - expected_away)
  
}


# Check results vs http://eloratings.net/
# team_ratings %>% arrange(rating)
team_ratings %>% filter(team %in% unique(c(fixtures$home_team, fixtures$away_team))) %>% arrange(-rating)


# Rn = Ro + K × (W - We)
# We = 1 / (10(-dr/400) + 1)
# -dr = diff in ratings (+ 100 for the team playing at home)



# Save output ----
teams <- team_ratings %>% filter(team %in% unique(c(fixtures$home_team, fixtures$away_team)))
save(teams, file="data/team_ratings.RData")
save(fixtures, file="data/fixtures.RData")                                 
