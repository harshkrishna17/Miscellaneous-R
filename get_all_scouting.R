# Libraries

library(worldfootballR)
library(purrr)

# Collect all team links 

pl <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
seriea <- fb_teams_urls("https://fbref.com/en/comps/11/Serie-A-Stats")
ligue1 <- fb_teams_urls("https://fbref.com/en/comps/13/Ligue-1-Stats")
bundesliga <- fb_teams_urls("https://fbref.com/en/comps/20/Bundesliga-Stats")
laliga <- fb_teams_urls("https://fbref.com/en/comps/12/La-Liga-Stats")

teams <- c(pl, seriea, ligue1, bundesliga, laliga)

# Collect player links from team links

players <- teams %>% 
  purrr::map(fb_player_urls)

# Convert list to vector

players <- purrr::flatten_chr(players)

# Function to extract player scouting reports from player links. 
# If FBref doesn't contain the data for a specific player, it will skip through that particular player. 

get_all_scouting <- function(vec) {
  f_possibly <- purrr::possibly(fb_player_scouting_report, otherwise = data.frame())
  purrr::map_dfr(vec, f_possibly)
}

# Apply the function

df <- get_all_scouting(vec = players)
