library(devtools)
devtools:::install_github("statsbomb/StatsBombR")
library(StatsBombR)
library(tidyverse)

dataframe <- FreeCompetitions() %>%
  filter(competition_id == 37 & season_name == "2020/2021")
df <- FreeMatches(dataframe)
StatsBombData <- StatsBombFreeEvents(MatchesDF = df, Parallel = T)
data <- allclean(StatsBombData)

df <- data %>%
  filter(match_id == 3764230) %>%
  filter(possession_team.name == "Tottenham Hotspur Women")

min_events <- df %>% 
  group_by(player.id) %>% 
  summarise(period = min(period), timestamp = min(timestamp)) %>% 
  na.omit() %>% 
  arrange(period, timestamp)

#' The next part is where the issue occurs. The purpose according to 
#' the source code is to filter events before time of first substitution. 
#' nrow(min_events) = 30

max_event <- min_events[12,]
idx <- which(df$period == max_event$period & df$timestamp == max_event$timestamp) - 1 ## idx is numeric (empty) 
df <- df[1:idx,] ## Error in 1:idx : argument of length 0