library(tidyverse)
library(gganimate)
library(understatr)
library(ggsoccer)
library(viridis)
library(gridExtra)
library(magick)
library(gifski)
library(ggbraid)
library(glue)

data <- get_player_shots(556)

data <- data %>%
  mutate(X = X * 120,
         Y = Y * 80)

shot_data <- data %>%
  filter(!situation == "Penalty")

p1 <- ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "black", colour = "white") +
  coord_flip(xlim = c(60,120),
             ylim = c(80, -2)) +
  theme_pitch() +
  stat_density_2d(data = shot_data, aes(x = X, y = Y, fill = ..level..), geom = "polygon",
                  alpha = 0.7, show.legend = FALSE) +
  scale_fill_viridis(option = "plasma") +
  theme(plot.background = element_rect(colour = "black", fill = "black"),
        panel.background = element_rect(colour = "black", fill = "black")) +
  gganimate::transition_time(year) +
  ease_aes("circular-in")

bar_data <- data %>%
  mutate(isGoal = ifelse(result == "Goal", 1, 0))

p2 <- ggplot(bar_data) +
  geom_bar(aes(x = isGoal, y = situation), stat = "identity", fill = "#3498DB", colour = "#3498DB") +
  theme(plot.background = element_rect(colour = "black", fill = "black"),
        panel.background = element_rect(colour = "black", fill = "black")) +
  theme(axis.title.x = element_text(colour = "white", face = "bold", size = 12),
        axis.title.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", size = 10),
        axis.text.y = element_text(colour = "white", size = 10)) +
  theme(panel.grid.major = element_line(colour = "#454545", size = 0.4, linetype = "dashed"),
        panel.grid.minor = element_line(colour = "#454545", size = 0.4, linetype = "dashed")) +
  theme(panel.grid.major.x = element_line(colour = "#454545", size = 0.4, linetype = "dashed"),
        panel.background = element_blank()) +
  labs(y = "Situation", x = "Goals") +
  gganimate::transition_time(year)

line_data <- data %>%
  mutate(isGoal = ifelse(result == "Goal", 1, 0)) %>%
  mutate(GxG = isGoal - xG) %>%
  mutate(GxGSM = TTR::SMA(GxG, n = 50)) %>%
  mutate(index = 1:nrow(data))

p3 <- ggplot(line_data, aes(x = index, y = GxGSM)) +
  geom_line(size = 2) +  
  geom_braid(aes(ymin = 0, ymax = GxGSM, fill = GxGSM > 0), show.legend = FALSE) +
  scale_fill_manual(values = c("#E74C3C", "#3498DB")) +
  geom_hline(yintercept = 0, size = 1, colour = "white", linetype = "longdash") +
  labs(title = "Marcus Rashford Shooting Profile : {round(frame_along, 0)}", x = "50 Shot Rolling Average", y = "G - xG") +
  theme(plot.background = element_rect(colour = "black", fill = "black"),
        panel.background = element_rect(colour = "black", fill = "black"),
        plot.title = element_text(colour = "white", size = 18, hjust = 0.5)) +
  theme(axis.title.x = element_text(colour = "white", face = "bold", size = 12),
        axis.title.y = element_text(colour = "white", face = "bold", size = 12),
        axis.text.x = element_text(colour = "white", size = 8),
        axis.text.y = element_text(colour = "white", size = 8)) +
  theme(panel.grid.major = element_line(colour = "#454545", size = 0.4, linetype = "dashed"),
        panel.grid.minor = element_line(colour = "#454545", size = 0.4, linetype = "dashed")) +
  theme(panel.grid.major.x = element_line(colour = "#454545", size = 0.4, linetype = "dashed"),
        panel.background = element_blank()) +
  gganimate::transition_reveal(year) +
  ease_aes("elastic-in")

p1_gif <- animate(p1, 
                  fps = 10, 
                  duration = 25,
                  width = 300, height = 200, 
                  renderer = magick_renderer())

p2_gif <- animate(p2, 
                  fps = 10, 
                  duration = 25,
                  width = 300, height = 200, 
                  renderer = magick_renderer())

p3_gif <- animate(p3, 
                  fps = 10, 
                  duration = 25,
                  width = 600, height = 250, 
                  renderer = magick_renderer())

combined <- image_append(c(p1_gif[1], p2_gif[1]))
new_gif <- image_append(c(p3_gif[1], image_flatten(combined)), stack=TRUE)

for(i in 2:250){
  combined <- image_append(c(p1_gif[i], p2_gif[i]))
  fullcombined <- image_append(c(p3_gif[i], image_flatten(combined)), stack=TRUE)
  new_gif <- c(new_gif, fullcombined)
}

image_write(new_gif, format = "gif", path = "animation.gif")
