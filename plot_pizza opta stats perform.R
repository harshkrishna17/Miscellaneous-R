library(tidyverse)
library(stringi)
library(stringr)

text_wrap <- function(x) {
  wrapped_text <- stri_wrap(x, width = 10, whitespace_only = TRUE, simplify = FALSE)
  final_text <- vapply(wrapped_text, stri_c, collapse = "\n", character(1))
  
  return(final_text) 
}

plot_pizza <- function(data, type = "", template, 
                       color_possession = "#41ab5d", color_attack = "#2171b5", 
                       color_defense = "#fec44f", 
                       player_1, player_2, 
                       color_compare = "#41ab5d", 
                       season = "Last 365 Days", 
                       season_player_1 = "Last 365 Days", 
                       season_player_2 = "Last 365 Days", 
                       theme = "") {
  
  if (theme == "dark" || theme == "") {
    fill_b <- "#0d1117"
    color_b <- "#0d1117"
    
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else if (theme == "black") {
    fill_b <- "black"
    color_b <- "black"
    
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else if (theme == "white") {
    fill_b <- "white"
    color_b <- "white"
    
    colorText <- "black"
    gridline <- "565656"
    colorLine <- "black"
  }
  
  if (type == "single" || type == "") { ## SINGLE PLOT ----
    
    data <- data %>%
      filter(scouting_period == season)
    
    data <- data %>% 
      mutate(stat = case_when(
        Statistic == "Non-Penalty Goals" |
          Statistic == "Non-Penalty xG" |
          Statistic == "Shots Total" |
          Statistic == "Assists" |
          Statistic == "xAG" |
          Statistic == "npxG + xAG" ~ "Attacking",
        Statistic == "Shot-Creating Actions" |
          Statistic == "Passes Attempted" |
          Statistic == "Pass Completion %" |
          Statistic == "Progressive Passes" |
          Statistic == "Dribbles Completed" |
          Statistic == "Touches (Att Pen)" |
          Statistic == "Progressive Passes Rec" ~ "Possession",
        Statistic == "Tackles" |
          Statistic == "Interceptions" |
          Statistic == "Blocks" |
          Statistic == "Clearances" |
          Statistic == "Aerials won" ~ "Defending",
        TRUE ~ NA_character_
      ))
    
    if (template == "outfielder") {
      
      data_selected <- data[c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18), ]
      
    }
    else if (template == "goalkeeper") {
      
      data_selected <- data[c(1, 2, 3, 4, 7, 8, 9, 10, 11, 12, 13), ]
      data_selected <- data_selected %>%
        mutate(stat = case_when(Statistic == "Goals Against" |
                                  Statistic == "PSxG/SoT" |
                                  Statistic == "Save Percentage" |
                                  Statistic == "PSxG-GA" ~ "Defending",
                                Statistic == "Touches" |
                                  Statistic == "Launch %" |
                                  Statistic == "Goal Kicks" |
                                  Statistic == "Avg. Length of Goal Kicks" ~ "Possession",
                                TRUE ~ "Attacking"))
      
    }
    else if (template == "custom") {
      
      data_selected <- data
      
    }
    
#    if (template == "forward") {
#      
#     if (nrow(data) > 148) {
#        
#        data_selected <- data[c(3, 8, 13, 24, 42, 128, 45, 115, 133, 107, 101, 102, 26, 147), ]
#        
#      } else {
#        
#        data_selected <- data[c(3, 8, 13, 24, 41, 127, 44, 114, 132, 106, 100, 101, 25, 146), ]
#        
#      }
#    } else if (template == "midfielder") {
#      
#      if (nrow(data) > 148) {
#        
#        data_selected <- data[c(3, 9, 10, 13, 53, 44, 47, 116, 125, 133, 146, 147, 107, 96), ]
#        
#      } else {
#        
#        data_selected <- data[c(3, 9, 10, 13, 52, 43, 46, 115, 124, 132, 145, 146, 106, 97), ]
#        
#      }
#    } else if (template == "defender") {
#      
#      if (nrow(data) > 148) {
#        
#        data_selected <- data[c(3, 11, 13, 44, 47, 129, 125, 110, 88, 96, 102, 106, 147, 108), ]
#        
#      } else {
#        
#        data_selected <- data[c(3, 11, 13, 43, 46, 128, 124, 109, 87, 95, 101, 105, 146, 107), ]
#        
#      }
#    } else if (template == "full back") {
#      
#      if (nrow(data) > 148) {
#        
#        data_selected <- data[c(3, 9, 10, 13, 114, 46, 47, 125, 43, 44, 147, 96, 107, 102), ]
#        
#      } else {
#        
#        data_selected <- data[c(3, 9, 10, 13, 113, 45, 46, 124, 42, 43, 146, 95, 106, 101), ]
#        
#      }
#    } else if (template == "winger") {
#      
#      if (nrow(data) > 148) {
#        
#        data_selected <- data[c(3, 22, 24, 42, 143, 45, 119, 47, 124, 133, 107, 146, 101, 102), ]
#        
#      } else {
#        
#        data_selected <- data[c(3, 21, 23, 41, 142, 44, 118, 46, 123, 132, 106, 145, 100, 101), ]
#        
#      }
#    } else if (template == "goalkeeper") {
#      
#      if (nrow(data) > 36) {
#        
#        data_selected <- data[c(4, 20, 22, 24, 26, 29, 35, 36, 37), ]
#        data_selected <- data_selected %>%
#          mutate(stat = case_when(Statistic == "Save%" |
#                                    Statistic == "PSxG" |
#                                    Statistic == "PSxG-GA" ~ "Defending",
#                                  Statistic == "Passes Attempted (Launched)" |
#                                    Statistic == "Passes Attempted" |
#                                    Statistic == "Average Pass Length" ~ "Possession",
#                                  TRUE ~ "Attacking"))
#      } else {
#        
#        data_selected <- data[c(4, 19, 21, 23, 25, 28, 34, 35, 36), ]
#        data_selected <- data_selected %>%
#          mutate(stat = case_when(Statistic == "Save%" |
#                                    Statistic == "PSxG" |
#                                    Statistic == "PSxG-GA" ~ "Defending",
#                                  Statistic == "Passes Attempted (Launched)" |
#                                    Statistic == "Passes Attempted" |
#                                    Statistic == "Average Pass Length" ~ "Possession",
#                                  TRUE ~ "Attacking"))
#      }
#    } else if (template == "custom") {
#      data_selected <- data
#    }
    
    player_name <- unique(data$Player)
    title <- paste(player_name, "Percentile Chart")
    min <- unique(data$BasedOnMinutes)
    sub <- unique(data$Versus)
    sub1 <- unique(data$scouting_period)
    subtitle <- paste("Compared to", sub, "|", sub1, "|", min, "minutes played")
    caption <- "Plot code by @RobinWilhelmus\nData from Stats Perform via FBref. Inspired by @NathanAClark. Created using ggshakeR."
    
    x <- c(data_selected$Statistic, data_selected$stat)
    
    data_selected <- data_selected %>%
      arrange(desc(stat), desc(Percentile)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    ggplot(data_selected, aes(Statistic, Percentile)) +
      geom_bar(aes(y = 100, fill = stat), stat = "identity", width = 1, color = fill_b,
               alpha = 0.1, show.legend = FALSE) +
      geom_bar(stat = "identity", width = 1, aes(fill = stat), color = fill_b, alpha = 1) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, color = colorLine, linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 50, color = colorLine, linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 75, color = colorLine, linetype = "dashed", alpha = 0.8) +
      scale_fill_manual(values = c("Possession" = color_possession,
                                   "Attacking" = color_attack,
                                   "Defending" = color_defense)) +
      geom_label(aes(y = 90, label = Per90, fill = stat), size = 3, color = fill_b, show.legend = FALSE) +
      scale_y_continuous(limits = c(-20, 100)) +
      labs(fill = "",
           caption = caption,
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = fill_b, color = color_b),
            panel.background = element_rect(fill = fill_b, color = color_b),
            legend.position = "bottom",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12, color = colorText),
            text = element_text(color = colorText, size = 20),
            plot.title = element_text(hjust = 0.5, size = 26, color = colorText, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 20, color = colorText),
            plot.caption = element_text(hjust = 0.5, size = 15, color = colorText),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) text_wrap(x = x))
    
  } else if (type == "comparison") { ## COMPARISON PLOT ----
    
    data$Player <- stri_trans_general(str = data$Player, id = "Latin-ASCII")
    
    data <- data %>% 
      mutate(stat = case_when(
        Statistic == "Non-Penalty Goals" |
          Statistic == "Non-Penalty xG" |
          Statistic == "Shots Total" |
          Statistic == "Assists" |
          Statistic == "xAG" |
          Statistic == "npxG + xAG" ~ "Attacking",
        Statistic == "Shot-Creating Actions" |
          Statistic == "Passes Attempted" |
          Statistic == "Pass Completion %" |
          Statistic == "Progressive Passes" |
          Statistic == "Dribbles Completed" |
          Statistic == "Touches (Att Pen)" |
          Statistic == "Progressive Passes Rec" ~ "Possession",
        Statistic == "Tackles" |
          Statistic == "Interceptions" |
          Statistic == "Blocks" |
          Statistic == "Clearances" |
          Statistic == "Aerials won" ~ "Defending",
        TRUE ~ NA_character_
      ))
    
    data1 <- data %>%
      filter(Player == player_1) %>%
      filter(scouting_period == season_player_1)
    data2 <- data %>%
      filter(Player == player_2) %>%
      filter(scouting_period == season_player_2)
    
    if (template == "outfielder") {
      
      data1 <- data1[c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18), ]
      data2 <- data2[c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18), ]
      
    }
    else if (template == "goalkeeper") {
      
      data1 <- data1[c(1, 2, 3, 4, 7, 8, 9, 10, 11, 12, 13), ]
      data2 <- data2[c(1, 2, 3, 4, 7, 8, 9, 10, 11, 12, 13), ]
      
    }
    else if (template == "custom") {
      
      data1
      data2
      
    }
    
#    if (template == "forward") {
#      
#      if (nrow(data1) > 148) {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 8, 13, 24, 42, 128, 45, 115, 133, 107, 101, 102, 26, 147), ]
#        data2 <- data2[c(3, 8, 13, 24, 42, 128, 45, 115, 133, 107, 101, 102, 26, 147), ]
#      } else {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 8, 13, 23, 41, 127, 44, 114, 132, 106, 100, 101, 25, 146), ]
#        data2 <- data2[c(3, 8, 13, 24, 42, 128, 45, 115, 133, 107, 101, 102, 26, 147), ]
#      }
#    } else if (template == "midfielder") {
#      
#      if (nrow(data1) > 148) {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 9, 10, 13, 53, 44, 47, 116, 125, 133, 146, 147, 107, 96), ]
#        data2 <- data2[c(3, 9, 10, 13, 53, 44, 47, 116, 125, 133, 146, 147, 107, 96), ]
#      } else {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 9, 10, 13, 52, 43, 46, 115, 124, 132, 145, 146, 106, 95), ]
#        data2 <- data2[c(3, 9, 10, 13, 53, 44, 47, 116, 125, 133, 146, 147, 107, 96), ]
#      }
#    } else if (template == "defender") {
#      
#      if (nrow(data1) > 148) {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 11, 13, 44, 47, 129, 125, 110, 88, 96, 102, 106, 147, 108), ]
#        data2 <- data2[c(3, 11, 13, 44, 47, 129, 125, 110, 88, 96, 102, 106, 147, 108), ]
#        
#      } else {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 11, 13, 43, 46, 128, 124, 109, 87, 95, 101, 105, 146, 107), ]
#        data2 <- data2[c(3, 11, 13, 44, 47, 129, 125, 110, 88, 96, 102, 106, 147, 108), ]
#      }
#    } else if (template == "full back") {
#      
#      if (nrow(data1) > 148) {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 9, 10, 13, 114, 46, 47, 125, 43, 44, 147, 96, 107, 102), ]
#        data2 <- data2[c(3, 9, 10, 13, 114, 46, 47, 125, 43, 44, 147, 96, 107, 102), ]
#      } else {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 9, 10, 13, 113, 45, 46, 124, 42, 43, 146, 95, 106, 101), ]
#        data2 <- data2[c(3, 9, 10, 13, 114, 46, 47, 125, 43, 44, 147, 96, 107, 102), ]
#      }
#    } else if (template == "winger") {
#      
#      if (nrow(data1) > 148) {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 22, 24, 42, 143, 45, 119, 47, 124, 133, 107, 146, 101, 102), ]
#        data2 <- data2[c(3, 22, 24, 42, 143, 45, 119, 47, 124, 133, 107, 146, 101, 102), ]
#      } else {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(3, 21, 23, 41, 142, 44, 118, 46, 123, 132, 106, 145, 100, 101), ]
#        data2 <- data2[c(3, 22, 24, 42, 143, 45, 119, 47, 124, 133, 107, 146, 101, 102), ]
#      }
#    } else if (template == "goalkeeper") {
#      
#     if (nrow(data1) > 36) {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(4, 20, 22, 24, 26, 29, 35, 36, 37), ]
#        data2 <- data2[c(4, 20, 22, 24, 26, 29, 35, 36, 37), ]
#      } else {
#        
#        data1$no <- 1:nrow(data1)
#        data2$no <- 1:nrow(data2)
#        data1 <- data1[c(4, 19, 21, 23, 25, 28, 34, 35, 36), ]
#        data2 <- data2[c(4, 20, 22, 24, 26, 29, 35, 36, 37), ]
#      }
#    } else if (template == "custom") {
#      data1
#      data2
#    }
    
    data2 <- data2 %>%
      rename(player = Player,
             per90 = Per90,
             percentile = Percentile)
    
    player_name1 <- unique(data1$Player)
    player_name2 <- unique(data2$player)
    min1 <- unique(data1$BasedOnMinutes)
    min2 <- unique(data2$BasedOnMinutes)
    sub <- unique(data1$Versus)
    lg1 <- unique(data1$scouting_period)
    lg2 <- unique(data2$scouting_period)
    title <- paste(player_name1, "|", lg1, "|", min1, "minutes")
    subtitle <- paste(player_name2, "|", lg2, "|", min2, "minutes")
    caption <- paste("Compared to", sub, ".\nData from Stats Perform via FBref. Inspired by @FootballSlices. Created using ggshakeR.")
    
    x <- data1$Statistic
    
    data1 <- data1 %>%
      arrange(desc(stat), desc(Percentile)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    data2 <- data2 %>%
      arrange(desc(stat), desc(percentile)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    ggplot(data1, aes(x = Statistic, y = Percentile)) +
      geom_bar(aes(y = 100), fill = fill_b, stat = "identity", width = 1, color = gridline,
               alpha = 0.5, show.legend = FALSE) +
      geom_bar(data = data1, aes(y = Percentile, fill = color_compare), color = color_compare, stat = "identity", width = 1, alpha = 1) +
      scale_fill_manual(values = color_compare) +
      geom_bar(data = data2, aes(y = percentile, fill = NA), stat = "identity", width = 1, alpha = 0, color = colorLine, size = 3) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, color = colorLine, linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 50, color = colorLine, linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 75, color = colorLine, linetype = "dashed", alpha = 0.7) +
      scale_y_continuous(limits = c(-20, 100)) +
      labs(caption = caption,
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = fill_b, color = color_b),
            panel.background = element_rect(fill = fill_b, color = color_b),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12, color = colorText),
            text = element_text(color = colorText, size = 20),
            plot.title = element_text(hjust = 0.5, size = 26, color = color_compare, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 26, color = colorLine, face = "bold"),
            plot.caption = element_text(hjust = 0.5, size = 15, color = colorText),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) text_wrap(x = x))
  }
}
