# Set working directory to the folder which has your data
# (the datasets sir sent through mail)

# use getwd() to find current working directory 

getwd()

# Set working directory to "downloads" (or whichever folder has the data)

setwd("C:/Users/harsh_1mwi2o4/Downloads")

# Reading .csv files 

data_csv <- read.csv("iris150.csv")

# Reading .txt files

data_txt <- read.table("iris150.txt", sep = ",", header = TRUE)

# Reading Excel files

# First create an .xlsx file in excel using the .csv data as sir did not send any excel file 

install.packages("readxl") # install package "readxl"
library(readxl) # load readxl

data_xl <- read_xlsx("iris150.xlsx")
