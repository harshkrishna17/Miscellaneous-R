# Libraries 

library(worldfootballR)

# Function

vector <- c("https://fbref.com/en/players/f586779e/Tammy-Abraham", "https://fbref.com/en/players/59e6e5bf/Dominic-Calvert-Lewin") 

get_all_scouting <- function(vec) {
  
  df <- data.frame(matrix(NA, nrow = 745, ncol = 8))
  
  for(i in 1:length(vec)) {
    
    df[, ] <- fb_player_scouting_report(vec[i], pos_versus = "primary")
  }
  
  return(df)
}

df <- get_all_scouting(vec = vector)
