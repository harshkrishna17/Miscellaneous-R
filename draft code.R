#libraries

library(tidyverse)
library(openxlsx)

#data

data <- read.csv("draft.csv")

#wrangling

data <- data %>%
  filter(positionText == "Defender") %>%
  select(name, playedPositionsShort, goal, assistTotal, 
         manOfTheMatch, teamName, teamRegionName)

data <- data %>%
  mutate(grouped_positions = case_when(playedPositionsShort == "D(R),M(R)" |
                                            playedPositionsShort == "D(R)" |
                                            playedPositionsShort == "D(R),DMC" |
                                            playedPositionsShort == "D(LR)" |
                                            playedPositionsShort == "D(L)" |
                                            playedPositionsShort == "D(LR),M(R)" |
                                            playedPositionsShort == "D(L),M(LR)" |
                                            playedPositionsShort == "D(L),M(L)" |
                                            playedPositionsShort == "D(R),M(L)" |
                                            playedPositionsShort == "D(LR),M(LR)" |
                                            playedPositionsShort == "M(R)" |
                                            playedPositionsShort == "M(L)" |
                                            playedPositionsShort == "D(LR),M(L)" ~ "Full Backs",
                                          playedPositionsShort == "D(CR),DMC" |
                                            playedPositionsShort == "D(CR),M(R)" |
                                            playedPositionsShort == "D(CR),DMC,M(R)" |
                                            playedPositionsShort == "D(CR)" |
                                            playedPositionsShort == "D(CLR)" |
                                            playedPositionsShort == "Defender" |
                                            playedPositionsShort == "D(CL)" |
                                            playedPositionsShort == "D(C)" |
                                            playedPositionsShort == "D(C),DMC" |
                                            playedPositionsShort == "D(C),M(R)" |
                                            playedPositionsShort == "DMC" |
                                            playedPositionsShort == "D(CL),DMC" ~ "Centre Backs",
                                          TRUE ~ playedPositionsShort))

list_data <- split(data, data$grouped_positions)

arrange_fun <- function(data) {
  df <- data %>%
    arrange(-assistTotal) %>%
    slice(1:50)
}

data_list <- list_data %>%
  purrr::map(arrange_fun)

#save as excel

wb <- createWorkbook()

Map(function(data, nameofsheet){     
  
  addWorksheet(wb, nameofsheet)
  writeData(wb, nameofsheet, data)
  
}, data_list, names(data_list))

saveWorkbook(wb, file = "draftdefenders.xlsx", overwrite = TRUE)
