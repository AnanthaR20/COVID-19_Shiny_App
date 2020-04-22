#Pipe 3 data functions

#Libraries
library(dplyr)
library(tidyr)

#--'Tracking Function'--#
# Returns a data.frame of state time series data
# Takes in a string of a state name or abbreviation
track2 <- function(st){
  if(tolower(st) %in% tolower(uskey$state)){
    abbr <- as.character(uskey$abb[tolower(uskey$state) == tolower(st)])
    
    
    df <- stts %>% filter(state == abbr)
    #add cap data
    df <- df %>% left_join(uskey[-3],by = c("state" = 'abb')) %>% 
      left_join(cap,by = c("state.y" = 'Location'))
    View(df)
    return(df)
  }
  abbr <- st
  df <- stts %>% filter(state == abbr)
  #add cap data
  df <- df %>% left_join(uskey[-3],by = c("state" = 'abb')) %>% 
    left_join(cap,by = c("state.y" = 'Location'))
  View(df)
  return(df)
}


#--'To Plottable' Formatting Function--#
# Takes data frames made by *Tracking Function* and
# puts them into a plottable format for ggplot
tp <- function(df){
  if("Location" %in% colnames(df)){
    df <- df %>% select(-Location) 
    return(df %>% pivot_longer(cols = colnames(df)[c(-1,-2)],names_to = "Rate_Type"))
  } else if("state.y" %in% colnames(df)){
    df <- df %>% select(-state.y) 
    return(df %>% pivot_longer(cols = colnames(df)[c(-1,-2)],names_to = "Rate_Type"))
  } else {
    return(df %>% pivot_longer(cols = colnames(df)[c(-1,-2)],names_to = "Rate_Type"))
  }
}



# A quick function to show the latest stats for the country
usnow <- function(){
  US_update <- fread("https://covidtracking.com/api/v1/us/current.csv")
  View(US_update)
  return(US_update)
}


# A quick function to show the latest stats for states
stnow <- function(){
  States_Update <- fread("https://covidtracking.com/api/v1/states/current.csv") %>% 
    select(state,positive,negative,recovered,death,hospitalizedCurrently,hospitalizedCumulative)
  View(States_Update)
  return(States_Update)
}


uskey <- read.csv("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/uskey.csv")


