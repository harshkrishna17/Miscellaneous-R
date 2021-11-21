# Libraries

library(worldfootballR)
library(tidyverse)

#Scraping

data <- understat_league_season_shots(league = "EPL", season_start_year = 2021)

# dplyr and tidyr magic

df1 <- data %>%
  filter(h_a == "h") %>%
  group_by(home_team, situation) %>%
  summarise(xGsum = sum(xG)) %>%
  spread(situation, xGsum) %>%
  ungroup()

df2 <- data %>%
  filter(h_a == "a") %>%
  group_by(away_team, situation) %>%
  summarise(xGsum = sum(xG)) %>%
  spread(situation, xGsum) %>%
  ungroup()

df3 <- data %>%
  filter(h_a == "h") %>%
  group_by(away_team, situation) %>%
  summarise(xGsum = sum(xG)) %>%
  spread(situation, xGsum) %>%
  ungroup()

df4 <- data %>%
  filter(h_a == "a") %>%
  group_by(home_team, situation) %>%
  summarise(xGsum = sum(xG)) %>%
  spread(situation, xGsum) %>%
  ungroup()

# NA values get out

df1[is.na(df1)] <- 0
df2[is.na(df2)] <- 0
df3[is.na(df3)] <- 0
df4[is.na(df4)] <- 0

# Basic addition and subtraction of multiple dataset columns

df1$DirectFreekick <- df1$DirectFreekick + df2$DirectFreekick
df1$FromCorner <- df1$FromCorner + df2$FromCorner
df1$OpenPlay <- df1$OpenPlay + df2$OpenPlay
df1$Penalty <- df1$Penalty + df2$Penalty
df1$SetPiece <- df1$SetPiece + df2$SetPiece

df3$DirectFreekick <- df3$DirectFreekick + df4$DirectFreekick
df3$FromCorner <- df3$FromCorner + df4$FromCorner
df3$OpenPlay <- df3$OpenPlay + df4$OpenPlay
df3$Penalty <- df3$Penalty + df4$Penalty
df3$SetPiece <- df3$SetPiece + df4$SetPiece

df1$DirectFreekick <- df1$DirectFreekick - df3$DirectFreekick
df1$FromCorner <- df1$FromCorner - df3$FromCorner
df1$OpenPlay <- df1$OpenPlay - df3$OpenPlay
df1$Penalty <- df1$Penalty - df3$Penalty
df1$SetPiece <- df1$SetPiece - df3$SetPiece

# Saving as .csv

write.csv(df1, "xgsplit.csv", row.names = FALSE)