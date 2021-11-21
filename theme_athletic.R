library(extrafont)

theme_athletic <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
          panel.background = element_rect(colour = "#151515", fill = "#151515")) +
    theme(plot.title = element_text(colour = "white", size = 24, family = "Suez One", hjust = 0.5),
          plot.subtitle = element_markdown(colour = "white", size = 18, family = "Suez One", hjust = 0.5),
          plot.caption = element_text(colour = "white", family = "Fried Chicken Bold", size = 40, hjust = 0),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_text(colour = "white", face = "bold", size = 14),
          axis.text.x = element_text(colour = "white", size = 12),
          axis.text.y = element_text(colour = "white", size = 12)) +
    theme(panel.grid.major = element_line(colour = "#525252", size = 0.7, linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#525252", size = 0.7, linetype = "dashed")) +
    theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.7, linetype = "dashed"),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}