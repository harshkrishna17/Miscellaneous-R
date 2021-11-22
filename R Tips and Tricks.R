# Install

install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
install.packages("tidyverse")
library(tidyverse)
devtools::install_github("abhiamishra/ggshakeR", subdir="ggshakeR")
library(ggshakeR)

# Scrape

data <- understat_league_season_shots(league = "EPL", season_start_year = 2021)

# Filtering 

data1 <- data %>%
  filter(situation == "OpenPlay")

data2 <- data %>%
  filter(situation == "OpenPlay" | situation == "FromCorner")

data3 <- data %>%
  filter(!situation == "Penalty")

data$minute <- as.numeric(data$minute)

data4 <- data %>%
  filter(minute >= 45)

data5 <- data %>%
  filter(minute <= 45)

data6 <- data %>%
  filter(minute > 20)

data7 <- data %>%
  filter(minute >= 45) %>%
  filter(situation == "OpenPlay") %>%
  filter(result == "Goal")

# New columns

data8 <- data %>%
  mutate(xGmean = mean(xG)) %>%
  mutate(meandiff = xGmean - xG)

data9 <- data %>%
  mutate(newcol = 90) %>%
  mutate(newcol2 = "Hi")

data10 <- data %>%
  rename(xGoals = xG)

# Ascending
data11 <- data[order(as.numeric(data$xG),decreasing = FALSE),]
# Descending
data12 <- data[order(as.numeric(data$xG),decreasing = TRUE),]

# Cleaning

df <- data ## to keep original dataset intact
df$minute[df$minute == 10] <- NA

## Replace with 0's
df[is.na(df)] <- 0

## Removing rows with NA's
df <- df %>%
  na.omit()

# Grouping

df1 <- data %>%
  group_by(player) %>%
  summarise(no = n(), xGsum = sum(xG))

# Basics

## dataframe <- dataframe[rows, columns]

## This selects the first 3 rows and all the columns
dataframe <- dataframe[c(1:3),]

# Binding together by rows
df <- rbind(data1, data2) ## number of rows will increase

# Binding together by columns 
df <- cbind(data1, data2) ## number of columns will increase

nrow(data) ## row
ncol(data) ## column

write.csv(df, "df.csv", row.names = FALSE)

# Plotting
## plot_shot

df <- data %>%
  filter(!situation == "Penalty") %>%
  filter(player == "Mohamed Salah")

plot_shot(df)
  
## plot_trendline

df1 <- data %>%
  filter(h_a == "h") %>%
  filter(home_team == "Manchester United") %>%
  group_by(date) %>%
  summarise(sum(xG))
df2 <- data %>%
  filter(h_a == "a") %>%
  filter(away_team == "Manchester United") %>%
  group_by(date) %>%
  summarise(sum(xG))
df <- rbind(df1, df2)

df3 <- data %>%
  filter(h_a == "a") %>%
  filter(home_team == "Manchester United") %>%
  group_by(date) %>%
  summarise(sum(xG))
df4 <- data %>%
  filter(h_a == "h") %>%
  filter(away_team == "Manchester United") %>%
  group_by(date) %>%
  summarise(sum(xG))
dfa <- rbind(df3, df4)
dfa <- dfa[, 2]
dfa <- dfa %>%
  rename(Away_xG = 'sum(xG)')

df <- cbind(df, dfa)
df <- df %>%
  rename(Home_xG = 'sum(xG)') %>%
  rename(Date = date) %>%
  mutate(Home = "Manchester United") %>%
  mutate(Away = "Misc")

df$Date <- as.Date(df$Date)

plot_trendline(data = df, team = "Manchester United",
               colour_xg = "#08519c", colour_xga = "#cb181d",
               roll_avg = 2, theme = "dark")