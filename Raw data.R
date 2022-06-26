title: "Data management assignment"
author: "Laeeqa Khan"
date: "25/04/2022"
dataset used: "transplants_29May2012.xls"

# Firstly lets load the relevent packages 

library(tidyverse)
library(readxl)

# Lets see what sheets we going to deal with in the Excel spreadsheet 

excel_sheets("~/University/UCT Honours/Data management/Git raw data/transplants_29May2012.xls")

# Read in data 

tp <- read_xls("~/University/UCT Honours/Data management/Git raw data/transplants_29May2012.xls", sheet = "dataframe")

# View data

tp

# Remove Na as this posed as a problem 

tp1 <- read_xls("~/University/UCT Honours/Data management/Git raw data/transplants_29May2012.xls", sheet = "dataframe", na = "NA")

# View data

tp1

#tp1 is a the raw data 