library(tidyverse)
library(ggsoccer)
library(glue)
library(patchwork)
library(cowplot)
library(RCurl)
library(jsonlite)
library(httr)
library(sp)
library(doParallel)
library(shiny)

Comp <- FreeCompetitions() %>%
  filter(competition_id == 16 & season_name == "2017/2018")
Matches <- FreeMatches(Comp)
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = TRUE)
data  <- allclean(StatsBombData)

ui <- fluidPage(
  titlePanel(div("Match Dashboard Creator", style = "color:#D81B60"), windowTitle = "Match Dashboard Creator"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("player", "Player Name:", choices = data$player.name),
      selectizeInput("plots", "Plots:", choices = c("All", "Heat Map", "Shot Map", "Pass Map", "Convex Hull")),
      radioButtons("theme", "Background Theme:", choices = c("Dark", "Light"), selected = "Dark"),
      downloadButton("download", "Download Plot")
    ),
    mainPanel(h2("Introduction & Plot", align = "center", style = "color:white"),
              h4("This simple Shiny app generates a dashboard of visualizations that can be useful in getting an overview of a soccer player's finishing ability. Play around with the options for customizations and try to gain interesting insights!", style = "color:white"),
              h5("Created by Harsh Krishna (@veryharshtakes)", style = "color:white"),
              plotOutput("plot"))
  )
)

server <- function(input, output, session) {
  
  plot_fun <- reactive({
    
    # Themes
    
    if (input$theme == "dark") {
      fill_b <- "#1 51515"
      color_b <- "white"
      color_line <- "white"
      color_text <- "white"
      sub_line <- "#656565"
    }
    
    else if (input$theme == "light") {
      fill_b <- "#EAEDED"
      color_b <- "black"
      color_line <- "black"
      color_text <- "black"
    }
    
    # Theme Function 
    
    theme_custom <- function() {
      
      theme_void() +
        theme(plot.background = element_rect(colour = fill_b, fill = fill_b),
              panel.background = element_rect(colour = fill_b, fill = fill_b),
              legend.background = element_rect(colour = fill_b, fill = fill_b),
              legend.key = element_rect(fill = fill_b, color = fill_b),
              legend.position = "top",
              legend.text = element_text(color = color_text),
              legend.title = element_text(color = color_text, face = "bold"),
              plot.title = element_text(size = 15, hjust = 0.5, color = sub_line, face = "bold"))
    }
    
    # Pitch 
    
    pitch <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, colour = color_b,
                     fill = fill_b) +
      theme_pitch() +
      theme(panel.background = element_rect(fill = fill_b))
    
    # Data Cleaning and Setting 
    
    data_shots <- data %>%
      filter(type.name == "Shot") %>%
      mutate(goal.outcome = if_else(shot.outcome.name == "Goal", "Goal", "No Goal")) %>%
      filter(player.name %in% player) %>%
      drop_na(c(shot.end_location.x, shot.end_location.y))
    
    data_pass <- data %>%
      filter(type.name == "Pass",
             player.name %in% input$player)
    
    data_pass$pass.outcome.name <- tidyr::replace_na(data_pass$pass.outcome.name, "Successful")
    data_pass <- data_pass %>% 
      mutate(pass.outcome.color = ifelse(pass.outcome.name == "Successful",
                                         "Successful",
                                         "Unsuccessful"))
    data_heat <- data %>%
      filter(player.name %in% player) %>%
      drop_na(c(location.x, location.y))
    
    data_hull <- data %>%
      filter(player.name %in% player) %>%
      drop_na(c(location.x, location.y))
    data_points <- data %>%
      filter(player.name %in% player) %>%
      drop_na(c(location.x, location.y))
    
    x_low <- quantile(data_hull$location.x, 0.05)
    x_high <- quantile(data_hull$location.x, 0.95)
    
    y_low <- quantile(data_hull$location.y, 0.05)
    y_high <- quantile(data_hull$location.y, 0.95)
    
    data_hull <- data_hull %>%  
      filter((location.x > x_low) & (location.x < x_high)) %>%
      filter((location.y > y_low) & (location.y < y_high)) %>%
      slice(chull(location.x, location.y))
    
    # Color Setting 
    
    if(data_heat$team.name[1] == "Real Madrid") {
      team_color <- "#F1C40F"
    }
    else if(data_heat$team.name[1] == "Liverpool") {
      team_color <- "#F44336"
    }
    
    # Plotting 
    # Shot Map
    
    total_shots <- nrow(data_shots)
    total_xg <- sum(data_shots$shot.statsbomb_xg)
    total_goals <- sum(data_shots$goal.outcome == "Goal")
    xg_shot <- total_xg / total_shots
    
    shot_map <- pitch +
      geom_point(data = data_shots, aes(x = location.x, y = location.y, 
                                        size = shot.statsbomb_xg, fill = goal.outcome, 
                                        alpha = goal.outcome),
                 colour = color_line, shape = 21, stroke = 1) +
      scale_fill_manual(values = c(team_color, fill_b)) +
      scale_alpha_manual(values = c(1, 0.7), guide = "none") +
      guides(fill = guide_legend(override.aes = list(size = 6))) +
      annotate(geom = "text", x = 70, y = 77, label = glue("Shots = {format(round(total_shots, 2))}"), colour = color_text, size = 4, fontface = "bold") +
      annotate(geom = "text", x = 70, y = 72, label = glue("Goals = {format(round(total_goals, 2))}"), colour = color_text, size = 4, fontface = "bold") +
      annotate(geom = "text", x = 75, y = 67, label = glue("xG/Shot = {format(round(xg_shot, 2))}"), colour = color_text, size = 4, fontface = "bold") +
      labs(fill = "Result",
           size = "xG",
           title = "Shot Map") +
      theme_custom()
    
    # Pass Map 
    
    pass_map <- pitch +
      geom_segment(data = data_pass, aes(x = location.x, y = location.y,
                                         xend = pass.end_location.x, yend = pass.end_location.y, 
                                         color = pass.outcome.color),
                   lineend = "butt", linejoin = "mitre", size = 1.2, arrow = arrow(length = unit(0.10, "inches")), 
                   stat = "identity", position = "identity") +
      scale_color_manual(values = c(team_color, sub_line)) +
      scale_alpha_manual(values = c(1, 0.7), guide = "none") +
      labs(color = "Outcome of Pass",
           title = "Pass Map") +
      theme_custom()
    
    # Heat Map 
    
    heat_map <- pitch +
      geom_bin2d(data = data_heat, aes(x = location.x, y = location.y),
                 binwidth = c(12, 8), color = color_line,
                 alpha = 0.9, show.legend = FALSE) +
      scale_fill_gradient(low = fill_b, high = team_color, name = "Count") +
      labs(title = "Heat Map") +
      theme_custom()
    
    # Hull Map 
    
    hull_map <- pitch +
      geom_point(data = data_points, aes(x = location.x, y = location.y), shape = 21, alpha = 0.5, fill = team_color, colour = color_line) +
      geom_polygon(data = data_hull, aes(x = location.x, y = location.y), colour = team_color, alpha = 0.3, fill = team_color, size = 0.7) +
      labs(title = "Convex Hull") +
      theme_custom()
    
    if ("All" %in% input$plots) {
      
      p_image <- pass_map + shot_map + heat_map + hull_map + plot_layout(nrow = 2, ncol = 2)
      final_image <- p_image +
        plot_annotation(title = player_name, 
                        subtitle = "Liverpool vs. Real Madrid (2017/2018)", 
                        theme = theme(plot.title = element_text(colour = color_text, size = 30, face = "bold"),
                                      plot.subtitle = element_text(colour = sub_line, size = 20, face = "bold"),
                                      plot.background = element_rect(colour = fill_b, fill = fill_b),
                                      panel.background = element_rect(colour = fill_b, fill = fill_b)))
      
      ggdraw(final_image, xlim = c(0,1), ylim = c(0,1))+
        draw_line(x = c(0.4, 0.6),
                  y = c(0.03, 0.03),
                  color = color_line, size = 1.2,
                  arrow = arrow(length = unit(0.12, "inches"), type = "closed"))+
        draw_text("Attacking Direction", x=0.5, y=0.015, colour = color_line, size = 10)
      
    } else if ("Shot Map" %in% input$plots) {
      
      shot_map
      
    } else if ("Pass Map" %in% input$plots) {
      
      pass_map
      
    } else if ("Heat Map" %in% input$plots) {
      
      heat_map
      
    } else if ("Convex Hull" %in% input$plots) {
      
      hull_map
      
    }
  })
  
  output$plot <- renderPlot({
    
    plot_fun()
    
  })
  
  # Download
  
  output$download <- downloadHandler(
    filename = function() { paste(input$player, ".png", sep="") },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 10, height = height, res = 300, units = "in")
      ggsave(file, plot = plot_fun(), device = device, bg = "#212121")
    }
  )
}

shinyApp(ui, server)

dashboard(player_name = "Karim Benzema", theme = "dark")
ggsave("Dashboard1.png", width = 3500, height = 2500, units = "px")
