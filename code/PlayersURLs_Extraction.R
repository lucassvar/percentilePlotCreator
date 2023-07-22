library(worldfootballR)
library(tidyverse)
library(dplyr)

# Function to extract the players' URLs and save them
# SAVED AS: "rda/URLs_data"
extract_playersURLs <- function(end_year = 2023){
  start_time <- Sys.time()
  # Empty dataframe to store the URLs
  URLs_data <- data.frame(
    League = character(),
    Team = character(),
    Player = character(),
    URL = character(),
    stringsAsFactors = FALSE
  )
  
  # Extract the Leagues URLs
  leaguesURLs <- c(fb_league_urls(country = c("ENG", "ESP", "FRA", "ITA", "GER", "POR", "NED"),
                                  gender = "M",
                                  season_end_year = end_year,
                                  tier = "1st"),
                   fb_league_urls(country = "ENG",
                                  gender = "M",
                                  season_end_year = end_year,
                                  tier = "2nd"))
  
  # Loop to extract the Teams & Players URLs
  for (league in leaguesURLs) {
    league_name <- gsub("-", " ", sub("-Stats", "", sub(".*/", "", league)))
    teamsURLs <- fb_teams_urls(league)
    for (team in teamsURLs) {
      team_name <- gsub("-", " ", sub("-Stats", "", sub(".*/", "", team)))
      playersURLs <- fb_player_urls(team)
      for (ply in playersURLs) {
        player_name <- gsub("-", " ", sub("-Stats", "", sub(".*/", "", ply)))
        
        # Create the new dataframe with the new row data
        new_row <- data.frame(League = league_name,
                              Team = team_name,
                              Player = player_name,
                              URL = ply,
                              stringsAsFactors = FALSE)
        
        # Add the new row to the original dataframe
        URLs_data <- rbind(URLs_data, new_row)
      }
    }
  }
  end_time <- Sys.time()
  print(difftime(start_time, end_time))
  save(URLs_data, file = "rda/URLs_data.rda")
}

