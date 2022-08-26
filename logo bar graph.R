library(tidyverse)
library(ggimage)
library(scales)

names <- c("Harsh", "Gautam", "Gopal", "Priya", "Abhi", 
           "Maram", "Ninad", "Hardik", "Himani", "Aadya",
           "Rithwik", "Ryo", "Thano", "Aaron", "Kees",
           "Alex", "Danny", "Hassan", "Mashir", "Shabab")
numbers <- c(1.81, 1.82, 1.83, 1.84, 1.85)
numbers <- rep_len(numbers, length.out = 20)

setwd("C:/Users/harsh_1mwi2o4/OneDrive/Documents/EPL Logos")
logos <- list.files()

data <- tibble(names = names,
               numbers = numbers,
               logos = logos)

data <- data %>%
  mutate(numbers = scales::rescale(numbers, to = c(0.01, 0.05)))

ggplot(data, aes(x = numbers, y = reorder(names, numbers))) +
  geom_image(aes(x = -0.004, image = logos), size = 0.04, asp = 8.1/9) +
  geom_bar(stat = "identity")
