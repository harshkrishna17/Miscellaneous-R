# Importing Libraries

library(tidyverse)
library(worldfootballR)

#' Scraping. The p90 function only works on the "fb_big5_advanced_season_stats" function 
#' from the worldfootballR package. Only for player's stats. Make sure to select only one season at a time.

data <- fb_big5_advanced_season_stats(season_end_year= 2022, stat_type= "shooting", team_or_player= "player")

#' Function. This particular function will calculate the per 90 values of each variable in the dataset.
#' It will also omit any NA values that occur due to the calculations.  

calc_p90 <- function(data) {

    df <- data[, c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Mins_Per_90")]
    Mins <- data$Mins_Per_90
    data <- subset(data, select = -c(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Mins_Per_90, Url))

    for(i in 1:ncol(data)) {        
        data[ , i] <- data[ , i] / Mins
    }

    data <- cbind(df, data)
    data <- data %>%
    na.omit()
}

data1 <- calc_p90(data)
