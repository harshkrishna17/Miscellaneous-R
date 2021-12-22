#' New Packages to load in

library(tidyverse)
library(understatr)
library(ggsoccer)
library(ggpubr)
library(gridExtra)

#' Function for plotting shots
#'
#' This function allows for data, that has to be scraped from Understat, to be used
#' for plotting shots.
#'
#' @param shotdata Dataframe that houses shot data. Dataframe must contain atleast the following columns: X,Y,xG,result,name
#' @param type Type of showcasing the shotmap: hexbin, density, point (default)
#' @param bin_size Bin size for creating bins. Use this when using hexbin shotmap. Default = 30.
#' @param theme Theme preferences for display: dark (default), white, rose, almond
#' @return a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @import ggsoccer
#'
#' @export
#'
#' @examples plot = plot_shot(shotdata, type+"hexbin", bin_size=20, avg_loc = TRUE, highlight_goals = FALSE)

plot_shot <- function(shotdata, type="", bin_size=30, highlight_goals = "", avg_loc = "", add_table = "", theme=""){

  if(nrow(shotdata)>0 &&
     sum(c("X","Y","xG","result","player") %in% names(shotdata))==5){

    last = sub(".* ", "", shotdata$player[nrow(shotdata)])
    first = sub(" .*", "", shotdata$player[nrow(shotdata)])
    player_name = paste(first,last,"Shot Map",sep="\n")

    fill_b = ""
    colour_b = ""
    colorLine = ""
    colorText = ""
    if(theme == "dark" || theme == ""){
      fill_b = "#0d1117"
      colour_b = "white"


      colorLine = "white"
      colorText = "white"
    }
    else if(theme == "white"){
      fill_b = "#F5F5F5"
      colour_b = "black"

      colorLine = "black"
      colorText = "black"
    }
    else if(theme == "rose"){
      fill_b = "#FFE4E1"
      colour_b = "#696969"

      colorLine = "#322E2E"
      colorText = "#322E2E"
    }
    else if(theme == "almond"){
      fill_b = "#FFEBCD"
      colour_b = "#696969"

      colorLine = "#322E2E"
      colorText = "#322E2E"
    }

    plot = shotdata %>%
      ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb,colour=colour_b,
                     fill = fill_b)+
      theme_pitch() +
      theme(panel.background = element_rect(fill = fill_b))


    if(nrow(shotdata)>=1){
      shotdata = shotdata %>%
        mutate(X = 120*X) %>%
        mutate(Y = 80*Y)

      total_xG = sum(shotdata$xG)
      total_goal = sum(shotdata$result == "Goal")
      xg_sot = total_xG/nrow(shotdata)

      if(avg_loc == TRUE || avg_loc == "") {

      if(type == "point" || type == ""){

          if(highlight_goals == FALSE || highlight_goals == "") {
        plot = plot +
          geom_point(data=shotdata, aes(x=X,y=(80-Y),size=xG,color=result),alpha=0.7)+
          scale_size_continuous(range = c(0.5,7))+
          geom_vline(xintercept = mean(shotdata$X), color=colorLine, linetype=2, size=1.5)+
          geom_hline(yintercept = mean(shotdata$Y), color=colorLine, linetype=2, size=1.5)+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          labs(
            color = "Result of Shot",
            size = "xG of Shot"
          )
    }
    else if(highlight_goals == TRUE) {

        shotdata <- shotdata %>%
        mutate(isGoal = ifelse(result == "Goal", "Goal", "No Goal"))

       plot = plot +
          geom_point(data=shotdata, aes(x=X,y=(80-Y),size=xG, fill = isGoal), color=colour_b, pch = 21, alpha=0.7)+
          scale_fill_manual(values = c("#2fc22f", fill_b)) +
          scale_size_continuous(range = c(0.5,7))+
          geom_vline(xintercept = mean(shotdata$X), color=colorLine, linetype=2, size=1.5)+
          geom_hline(yintercept = mean(shotdata$Y), color=colorLine, linetype=2, size=1.5)+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          labs(
            color = "Result of Shot",
            size = "xG of Shot"
          )
         } 
        }
        else if(type == "density"){
          plot = plot +
          stat_density_2d(data=shotdata, aes(x=X,y=(80-Y),fill = ..level..), geom = "polygon",
                          alpha=0.7)+
          scale_fill_gradient(high="#6BFF84",low="#01141D")+
          scale_size_continuous(range = c(0.5,7))+
          geom_vline(xintercept = mean(shotdata$X), color=colorLine, linetype=2, size=1.5)+
          geom_hline(yintercept = mean(shotdata$Y), color=colorLine, linetype=2, size=1.5)+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          theme(legend.position = "none")

      }
      else if(type == "hexbin"){
        plot = plot +
          geom_hex(data=shotdata, aes(x=X,y=(80-Y)), bins=bin_size)+
          scale_fill_continuous(type = "viridis")+
          scale_size_continuous(range = c(0.5,7))+
          geom_vline(xintercept = mean(shotdata$X), color=colorLine, linetype=2, size=1.5)+
          geom_hline(yintercept = mean(shotdata$Y), color=colorLine, linetype=2, size=1.5)+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          labs(
            fill = "Count of Shots"
          )
      }
    }
    else if(avg_loc == FALSE) {

       if(type == "point" || type == ""){

          if(highlight_goals == FALSE || highlight_goals == "") {
        plot = plot +
          geom_point(data=shotdata, aes(x=X,y=(80-Y),size=xG,color=result),alpha=0.7)+
          scale_size_continuous(range = c(0.5,7))+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          labs(
            color = "Result of Shot",
            size = "xG of Shot"
          )
    }
    else if(highlight_goals == TRUE) {

        shotdata <- shotdata %>%
        mutate(isGoal = ifelse(result == "Goal", "Goal", "No Goal"))

       plot = plot +
          geom_point(data=shotdata, aes(x=X,y=(80-Y),size=xG, fill = isGoal), color=colour_b, pch = 21, alpha=0.7)+
          scale_fill_manual(values = c("#2fc22f", fill_b)) +
          scale_size_continuous(range = c(0.5,7))+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          labs(
            color = "Result of Shot",
            size = "xG of Shot"
          )
         } 
        }
        else if(type == "density"){
          plot = plot +
          stat_density_2d(data=shotdata, aes(x=X,y=(80-Y),fill = ..level..), geom = "polygon",
                          alpha=0.7)+
          scale_fill_gradient(high="#6BFF84",low="#01141D")+
          scale_size_continuous(range = c(0.5,7))+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          theme(legend.position = "none")

      }
      else if(type == "hexbin"){
        plot = plot +
          geom_hex(data=shotdata, aes(x=X,y=(80-Y)), bins=bin_size)+
          scale_fill_continuous(type = "viridis")+
          scale_size_continuous(range = c(0.5,7))+
          geom_point(x=86,y=10,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=10,label=format(round(total_xG,2)),color=colorText,size=10)+
          geom_text(x=80,y=10,label="xG",color=colorText,size=10)+
          geom_point(x=86,y=70,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=70,label=format(round(total_goal,2)),color=colorText,size=10)+
          geom_text(x=80,y=70,label="Goals",color=colorText,size=10)+
          geom_point(x=86,y=40,size=40, color=colorText, shape=1)+
          geom_text(x=86,y=40,label=format(round(xg_sot,2)),color=colorText,size=10)+
          geom_text(x=80,y=40,label="xG/Shot",color=colorText,size=10)+
          coord_flip(xlim = c(80, 120),
                     ylim = c(0, 80))+
          labs(
            fill = "Count of Shots"
          )
      } 
    }

    if(add_table == FALSE || add_table == "") {

      plot = plot +
        geom_text(x=110,y=10,label=player_name,color=colorText,size=6)
    }
    else if(add_table == TRUE) {

      plot = plot +
        geom_text(x=110,y=10,label=player_name,color=colorText,size=6)

shotdata <- shotdata %>%
  group_by(shotType, situation) %>%
  summarise(xGsum = sum(xG)) %>%
  spread(situation, xGsum) %>%
  ungroup() %>%
  rename("Shot Type" = shotType)

shotdata[is.na(shotdata)] <- 0

table <- shotdata %>%
mutate(across(where(is.numeric), ~ round(., 2)))

  tt3 <- ttheme_minimal(
  core=list(bg_params = list(col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="black", fontface=4L)),
  rowhead=list(fg_params=list(col="white", fontface=3L)))

table <- ggplot() +
theme_void() +
annotation_custom(tableGrob(table, theme = tt3), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

plot = ggarrange(plot, table, nrow = 2, ncol = 1)
    plot
  }
}
else{
      plot
    }
  }
}
