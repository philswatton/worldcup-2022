# GV2022 World Cup Predictions
# Phil Swatton
# University of Essex
# File 03: Results


# Packages
library(tidyverse)
library(combinat)
library(htmlTable)


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



# Game Results ----
results_group_fixtures <- group_probs %>%
  select(home_team, away_team, group, home_win_prob = home_win)
write_csv(results_group_fixtures, file="results/results_01_group_fixtures.csv")


# Group Rank Orders ----

# with teams a, b, c, d, there are 24 possible rank orders
# use combinat::permn(x), where x is the vector of teams

expected_group_order <- map_dfr(results_group, function(x) {
  # Unique Orders
  orders <- permn(unique(x[1,])) %>% reduce(rbind)
  
  # Count of each unique order occurrence
  order_count <- vector(mode="double", length=24)
  for (i in 1:N) {
    index <- which(orders[,1] == x[i,1] & orders[,2] == x[i,2] & orders[,3] == x[i,3] & orders[,4] == x[i,4])
    order_count[index] <- order_count[index] + 1
    
  }
  
  # Predicted order and probability
  prediction <- data.frame(
    Order = str_c(orders[which.max(order_count),], collapse=", "),
    Probability = round(order_count[which.max(order_count)]/N*100,1)
  )
  
  return(prediction)
})
rownames(expected_group_order) <- str_c("Group ", c("A","B","C","D","E","F","G","H"))
expected_group_order
write_csv(expected_group_order, "results/results_02_expected_group_order.csv")



# Team probabilities ----

## Make it to 16 stage
made16 <- map(results_group, function(x) {
  return(x[,1:2])
}) %>%
  reduce(rbind) %>%
  table() %>%
  as.data.frame()
names(made16) <- c("Team", "Last 16")


## 16s Winners
won16 <- table(results_16) %>%
  as.data.frame()
names(won16) <- c("Team", "Quarter Finals")


## Quarters Winners
wonQuarters <- table(results_quarter) %>%
  as.data.frame()
names(wonQuarters) <- c("Team", "Semi Finals")


## Semi Winners
wonSemis <- table(results_semi) %>%
  as.data.frame()
names(wonSemis) <- c("Team", "Finals")


## Final Winners
wonFinal <- table(results_final) %>%
  as.data.frame()
names(wonFinal) <- c("Team", "Win")


## Joining Up
expected_teams <- left_join(made16, won16, by="Team") %>%
  left_join(wonQuarters, by="Team") %>%
  left_join(wonSemis, by="Team") %>%
  left_join(wonFinal, by="Team") %>%
  mutate(across(where(is.numeric), ~round(.x/N*100,1))) %>%
  arrange(-`Finals`, -`Finals`, -`Semi Finals`)
expected_teams
write_csv(expected_teams, "results/results_03_expected_team_performances.csv")



# HTML Outputs ----

# Funcion for colouring cells
# https://stackoverflow.com/a/38639979
cell_colour <- function(df) {
  
  apply(df,
        c(1,2),
        function(i) {
          i <- 1 - (i/100)
          paste0("background-color:RGB(", round(i*255,0), ", ", "255", ", " , round(i*255,0) ,")")
        })
  
}


# Expected Rank Order
order_table <- htmlTable(expected_group_order)
print(order_table, useViewer = F)
writeLines(print(order_table, useViewer = F), "results/results_02_expected_group_order.html")


# Expected Team Results
html_teams <- expected_teams[-1]
rownames(html_teams) <- expected_teams$Team
expected_table <- htmlTable(html_teams,
          css.cell=cell_colour(html_teams))
writeLines(print(expected_table, useViewer = F), "results/results_03_expected_team_performances.html")




# Quick calculation of expected pts in essex contest ----

# Teams implicitly predicted by rank order:
rank_teams <- c("Netherlands", "Ecuador", "England", "Iran", "Argentina", "Mexico", "France", "Denmark",
  "Spain", "Germany", "Belgium", "Croatia", "Brazil", "Switzerland", "Portugal", "Uruguay")
all((made16 %>% arrange(-`Last 16`) %>% `[[`("Team") %>% `[`(1:16)) %in% rank_teams) #TRUE - nice!

# Calculate pts for each stage of contest
pts1 <- expected_group_order$Probability/100 * 2
pts2 <- ((made16 %>% arrange(-`Last 16`) %>% `[[`(2))/N)[1:16]
pts3 <- ((won16 %>% arrange(-`Quarter Finals`) %>% `[[`(2))/N)[1:8] * 2
pts4 <- ((wonQuarters %>% arrange(-`Semi Finals`) %>% `[[`(2))/N)[1:4] * 2
pts5 <- ((wonSemis %>% arrange(-`Finals`) %>% `[[`(2))/N)[1:2] * 2
pts6 <- ((wonFinal %>% arrange(-`Win`) %>% `[[`(2))/N)[1] * 3

# Total
sum(pts1) + sum(pts2) + sum(pts3) + sum(pts4) + sum(pts5) + sum(pts6)


