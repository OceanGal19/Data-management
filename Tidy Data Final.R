title: "GIT Assignment"
author: "Laeeqa Khan"
date: "25/04/2022"

# Load the relevent data and packages 

library(tidyverse)
library(readxl) 

# See what sheets we going to deal with in the Excel spreadsheet 

excel_sheets("~/University/UCT Honours/Data management/Git raw data/transplants_29May2012.xls")

# Read in data 

tp <- read_xls("~/University/UCT Honours/Data management/Git raw data/transplants_29May2012.xls", sheet = "dataframe")

# View data

tp

# Remove Na as this posed as a problem 

tp1 <- read_xls("~/University/UCT Honours/Data management/Git raw data/transplants_29May2012.xls", sheet = "dataframe", na = "NA")

# View data

tp1 

# Lets start Tidyng #

# In the raw data, the site column has two components Site and type of sediment. Lets seperate them into its individual column 

(tpp <- (tp1 %>% mutate(SiteID = word(Site,2), .before = Site)) %>% 
    mutate(`Sediment Type` = str_sub(word(tp1$Site,3),2,-2), .after = SiteID) %>% 
    mutate(Site = NULL)) 

# Lets rename the columns for easier interpretation 

{(TP_tidy <- tpp %>% 
    rename(`Height x longest canopy axis  in 1994 (cm^2)` = `cm2 in 1994`, `Standard dev in 1994 (cm^2)` = `std (cm2) in 1994`, `Number of individuals in 1994` = `n in 1994`,
           `Height x longest canopy axis  in 2008 (cm^2)` = `cm2 in 2008`, `Standard dev in 2008 (cm^2)` = `std (cm2) in 2008`, `Number of individuals in 2008` = `n in 2008`,
           `Height x longest canopy axis  in 2012 (cm^2)` = `cm2 in 2012`,  `Standard dev in 2012 (cm^2)` = `std (cm2) in 2012`, `Number of individuals in 2012` = `n in 2012`)  %>% 
    
    # Now we will use pivot longer function to turn the data into long format 
    
    pivot_longer(cols = c(`Height x longest canopy axis  in 1994 (cm^2)`, `Standard dev in 1994 (cm^2)`, `Number of individuals in 1994`,
                          `Height x longest canopy axis  in 2008 (cm^2)`, `Standard dev in 2008 (cm^2)`, `Number of individuals in 2008`,
                          `Height x longest canopy axis  in 2012 (cm^2)`, `Standard dev in 2012 (cm^2)`, `Number of individuals in 2012`),
                 names_to = "parameter", values_to = "value") %>% relocate(Species) %>% 
    
    # Group the datafrom by species and arrange it by species    
    
    rename(Parameter = parameter, Value = value, `Plant growth type` = Type)) %>%  
    group_by(Species) %>% 
    arrange(Species)}

# Since each string variable has various categories that are replicated, we are going convert them all to factors


TP_tidy[sapply(TP_tidy, is.character)] <- lapply(TP_tidy[sapply(TP_tidy, is.character)], as.factor) 
TP_tidy$Parameter <- as.character(TP_tidy$Parameter)    

TP_tidy %>% select_if(is.factor) %>% sapply(levels)

### Now the data is Tidy ###

### Now its time to filter the data for results

# Mean for all biomes 
MeanBiome <- TP_tidy %>% 
  group_by(Biome, Parameter) %>% 
  summarize(`Mean measurements` = mean(Value))


# Total number of individuals found in each biome
(tpp_number <- TP_tidy %>% 
    group_by(`Plant growth type`) %>% 
    summarize(`Total number of species` = sum(Value[Parameter])))
              
# Mean value of Proteas              
tpp_protea <- TP_tidy %>% 
  group_by(Species) %>% filter(`Plant growth type`== "Protea") %>% 
  summarise(`Mean Protea area cover (cm^2)` = mean(Value[Parameter]))
  
# Mean value of restios 
  
tpp_restio <- TP_tidy %>% 
      group_by(SiteID) %>% filter(`Plant growth type`== "Restio") %>%
      summarise(`Mean Restio area cover (cm^2)` = mean(Value[Parameter]))
                
# Mean value of ericoid

tpp_ericoid <- TP_tidy %>% 
  group_by(SiteID) %>% filter(`Plant growth type`== "Ericoid") %>%
  summarise(`Mean Ericoid area cover (cm^2)` = mean(Value[Parameter]))

# Lets create a plot showing all the variables according to site 

library(ggplot2)
ggplot(TP_tidy)+
  geom_boxplot(aes(y = Value, x = SiteID)) 

ggplot(TP_tidy)+
  geom_boxplot(aes(y = Value, x = Biome)) 

ggplot(TP_tidy) +
  geom_boxplot(aes(y = Value, x = SiteID)) +
    facet_wrap(vars(Parameter), scales = "free_y")


