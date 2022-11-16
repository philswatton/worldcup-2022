# GV2022 World Cup Predictions
# Phil Swatton
# University of Essex
# File 01: Elo Rankings


# Packages
library(tidyverse)
library(lubridate)


# Data (see README for links)
matches <- read_csv("data/results.csv")
fixtures <- read_csv("data/fifa-world-cup-2022-GMTStandardTime.csv")



# Add importance weights to matches
# Somewhat based on:
# - http://eloratings.net/about for values
# - https://www.kaggle.com/code/lekroll/predictions-for-the-fifa-world-cup-2018-using-r/notebook for use of regex
matches <- matches %>%
  mutate(importance = case_when(str_detect(tournament,"FIFA") ~ 60,
                                str_detect(tournament, "UEFA") ~ 50,
                                str_detect(tournament, "Copa América") | str_detect(tournament, "African Cup of Nations") ~ 40,
                                !str_detect(tournament, "Friendly") ~ 30,
                                str_detect(tournament, "Friendly") ~ 20),
         importance = case_when(str_detect(tournament, "qualification") ~ importance * 0.75,
                                T ~ importance))




# Elo Start ----
team_scores <- data.frame(
  team = unique(c(matches$home_team, matches$away_team)),
  score = 1500
)




# Elo Algo ----


# Rn = Ro + K × (W - We)
# We = 