#' Function for plotting pass networks
#'
#' This function allows for data, that can be from Opta or Statsbomb, to be used
#' for plotting pass networks.
#'
#' @param eventData Dataframe that houses pass data
#' @param dataType Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param team_name The name of the team of which you want a pass network
#' @param subtitlePlot Subtitle of the pass network plot
#' @param theme The background theme -> "light" or "dark"
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import tidyr
#' @import ggsoccer
#' @import viridis
#' @import gridExtra
#' @import glue
#' @import useful
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_passnet(eventData, dataType = "statsbomb", team_name = "Arsenal", theme = "dark", subtitlePlot = "Optional Subtitle")
#' plot
#' }

plot_passnet <- function(eventData, dataType = "statsbomb", 
                         team_name, subtitlePlot = "", theme = "") {
  
  if(theme == "dark"|| theme == "") {
    fill_b <- "#0d1117"
    colour_b <- "#0d1117"
    
    colorText <- "white"
    colorLine <- "white"
  } 
  else if(theme == "light") {
    fill_b <- "floral white"
    colour_b <- "floral white"
    
    colorText <- "black"
    colorLine <- "black"
  }
    
  if(dataType == "statsbomb") {
    
    df <- eventData %>%
      rename("x" = "location.x",
             "y" = "location.y",
             "finalX" = "pass.end_location.x",
             "finalY" = "pass.end_location.y") %>%
      ggshakeR::calculate_threat(dataType = "statsbomb")
    
    df <- df %>%
      mutate(xT = xTEnd - xTStart) %>%
      select(xT)
    df[is.na(df)] <- 0
    
    eventData$xT <- df$xT
  
    # Data Wrangling
    
    data1 <- eventData %>%
      filter(team.name == team_name)
    
    # Filter to before first substitution
    
    min_events <- data1 %>%
      filter(type.name == "Substitution")
    
    min <- min(min_events$minute)
    
    data1 <- data1 %>%
      filter(minute < min)
    
    # Player Average Locations
    
    nodes <- data1 %>% 
      filter(type.name %in% c("Pass", "Ball Receipt*", "Ball Recovery", "Shot", "Dispossessed", "Interception", "Clearance", "Dribble", "Shot", "Goal Keeper", "Miscontrol", "Error")) %>% 
      group_by(player.name) %>% 
      summarise(x = mean(location.x, na.rm=T), y = mean(location.y, na.rm=T), events = n(), xT = sum(xT)) %>% 
      na.omit()
    
    # Edges
    
    edgelist <- data1 %>% 
      mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
      filter(type.name == "Pass" & pass.outcome.name == "Complete") %>% 
      select(from = player.name, to = pass.recipient.name) %>% 
      group_by(from, to) %>% 
      summarise(n = n()) %>% 
      na.omit()
    
    edges <- left_join(edgelist, 
                       nodes %>% select(player.name, x, y),
                       by = c("from" = "player.name"))
    
    edges <- left_join(edges, 
                       nodes %>% select(player.name, xend = x, yend = y),
                       by = c("to" = "player.name"))
    
    edges <- edges %>% 
      group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
      summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
    
    # Minimum Number of Connections
    
    edges <- edges %>% 
      filter(n >= 3)
    
    # Creating Line-up Table
    
    nodes <- nodes %>%
      arrange(events)
    
    nodes$id <- 1:nrow(nodes) 
    nodes$player.name <- sub(".* ", "", nodes$player.name)
    
    table <- nodes %>%
      select(id, player.name) %>%
      rename(Player = player.name) %>%
      rename("#" = id)
    
    # Plot 
    
    if(subtitlePlot == "") {
      subtitlePlot <- "Single Team"
    }
    
    plot_passnet <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = fill_b, colour = colorLine) +
      theme_pitch() +
      geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, alpha = n), colour = colorLine, show.legend = FALSE, size = 2.5) +
      geom_point(data = nodes, aes(x, y, fill = xT), size = 9, shape = 21, stroke = 1, colour = colorLine) +
      scale_fill_viridis_c(option = "viridis") +
      geom_text(data = nodes, aes(x, y, label = id), colour = colorText) +
      annotate(geom = "text", x = 5.5, y = 14, label = "Only 3+ Pass Connections.\nOpacity = Number of connections", size = 2.5, colour = colorText) +
      labs(title = glue("{team_name} Pass Network"),
           subtitle = subtitlePlot,
           caption = "Data from StatsBomb\nCreated using ggshakeR") +
      theme(legend.position = c(0.88, 1.06),
            legend.direction = "horizontal",
            legend.background = element_rect(fill = fill_b),
            legend.title = element_text(colour = colorText),
            legend.text = element_text(colour = colorText),
            plot.background = element_rect(colour = fill_b, fill = fill_b),
            panel.background = element_rect(colour = fill_b, fill = fill_b),
            plot.title = element_text(colour = colorText, hjust = 0.5, size = 25, face = "bold"),
            plot.caption = element_text(colour = colorText, size = 10),
            plot.subtitle = element_text(colour = colorText, hjust = 0.5, size = 14)) +
      coord_flip(ylim = c(-25,100),
                 xlim = c(0,120)) +
      annotation_custom(tableGrob(table, theme = ttheme_minimal(base_colour = colorText), rows = NULL), xmin=0, xmax=120, ymin=62, ymax=125) +
      direction_label(colour = colorLine, x_label = 107, y_label = 10)
  }
  
  else if(dataType == "opta") {
    
    # Generate Error Messages
    
    if(is.character(eventData$playerId) == FALSE) {
      stop("The playerId column is supposed to contain player names.")
    } else {
      
    }
    
    if(is.character(eventData$teamId) == FALSE) {
      stop("The teamId column is supposed to contain team names.")
    } else {
      
    }
    
    # Checking dataframe 
    
    if((nrow(eventData) > 0) &&
        sum(c("x","y","endX","endY", "teamId", "playerId", "type", "minute") %in% names(eventData)) == 8 &&
         (dataType == "statsbomb" || dataType == "opta")) {
      
    } else {
      stop("The dataframe does not contain the required columns")
      
    }
    
    # Calculate xT 
    
    df <- eventData %>%
      rename("x" = "x",
             "y" = "y",
             "finalX" = "endX",
             "finalY" = "endY") %>%
      ggshakeR::calculate_threat(dataType = "opta")
    
    df <- df %>%
      mutate(xT = xTEnd - xTStart) %>%
      select(xT)
    df[is.na(df)] <- 0
    
    eventData$xT <- df$xT
    
    # Data Wrangling 
    
    data1 <- eventData %>%
      mutate(x = x * 1.2) %>%
      mutate(y = y * 0.8) %>%
      mutate(endX = endX * 1.2) %>%
      mutate(endY = endY * 0.8) %>%
      filter(teamId == team_name)
    
    data1 <- data1[complete.cases(data1[ , "playerId"]), ]
    data1 <- shift.column(data = data1, columns = "playerId", newNames = "receiver", len = 1, up = TRUE)
    
    # Filter to before first substitution
    
    min_events <- data1 %>% 
      filter(type == "SubstitutionOff") %>%
      arrange(minute)
    
    min <- min(min_events$minute)
    
    data1 <- data1 %>%
      filter(minute < min)
    
    # Player Average Locations
    
    nodes <- data1 %>% 
      group_by(playerId) %>% 
      summarise(x = mean(x, na.rm=T), y = mean(y, na.rm=T), events = n(), xT = sum(xT)) %>% 
      na.omit()
    
    # Edges
    
    edgelist <- data1 %>% 
      filter(type == "Pass" & outcome == "Successful") %>% 
      select(from = playerId, to = receiver) %>% 
      group_by(from, to) %>% 
      summarise(n = n()) %>% 
      na.omit()
    
    edges <- left_join(edgelist, 
                       nodes %>% select(playerId, x, y),
                       by = c("from" = "playerId"))
    
    edges <- left_join(edges, 
                       nodes %>% select(playerId, xend = x, yend = y),
                       by = c("to" = "playerId"))
    
    edges <- edges %>% 
      group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
      summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
    
    # Minimum Number of Connections
    
    edges <- edges %>% 
      filter(n >= 3)
    
    # Create Line-up Table
    
    nodes <- nodes %>%
      arrange(events)
    
    nodes$id <- 1:nrow(nodes) 
    nodes$playerId <- sub(".* ", "", nodes$playerId)
    
    table <- nodes %>%
      select(id, playerId) %>%
      rename(Player = playerId) %>%
      rename("#" = id)
    
    # Plot 
    
    if(subtitlePlot == "") {
      subtitlePlot <- "Single Team"
    }
    
    plot_passnet <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = fill_b, colour = colorLine) +
      theme_pitch() +
      geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, alpha = n), colour = colorLine, show.legend = FALSE, size = 2.5) +
      geom_point(data = nodes, aes(x, y, fill = xT), size = 9, shape = 21, stroke = 1, colour = colorLine) +
      scale_fill_viridis_c(option = "viridis") +
      geom_text(data = nodes, aes(x, y, label = id), colour = colorText) +
      annotate(geom = "text", x = 5.5, y = 14, label = "Only 3+ Pass Connections.\nOpacity = Number of connections", size = 2.5, colour = colorText) +
      labs(title = glue("{team_name} Pass Network"),
           subtitle = subtitlePlot,
           caption = "Created using ggshakeR") +
      theme(legend.position = c(0.88, 1.06),
            legend.direction = "horizontal",
            legend.background = element_rect(fill = fill_b),
            legend.title = element_text(colour = colorText),
            legend.text = element_text(colour = colorText),
            plot.background = element_rect(colour = fill_b, fill = fill_b),
            panel.background = element_rect(colour = fill_b, fill = fill_b),
            plot.title = element_text(colour = colorText, hjust = 0.5, size = 25, face = "bold"),
            plot.caption = element_text(colour = colorText, size = 10),
            plot.subtitle = element_text(colour = colorText, hjust = 0.5, size = 14)) +
      coord_flip(ylim = c(100,-25),
                 xlim = c(0,120)) +
      annotation_custom(tableGrob(table, theme = ttheme_minimal(base_colour = colorText), rows = NULL), xmin=0, xmax=120, ymin=125, ymax=-155) +
      direction_label(colour = colorLine, x_label = 107, y_label = 10)
  }
  return(plot_passnet)
}
