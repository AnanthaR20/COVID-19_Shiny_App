# ::::::::::Functions for use :::::::::: #
# This script must be run at the end of 'pipe1.R'
# to initialize the functions properly
# ::::::::::::::::::::::::::::::::::::: #
print("Running pipe1functions.R")
#--Time Slice Function--#
# returns data from t days ago
#
t <- function(days = 0){
  return(world[[length(world)-days]])
}


#--Add Incidence Function--#
# returns df with an incidence column
addIncidence <- function(df){
  for(i in 1:nrow(df)){
    if(i == 1){
      df$incidence[i] <- df$confirms[i]
      df$rate[i] <- 1
    }
    else {
      df$incidence[i] <- df$confirms[i]-df$confirms[i-1]
      df$rate[i] <- df$confirms[i]/df$confirms[i-1]
    }
  }
  return(df)
}

#--'To Plottable' Formatting Function--#
# Takes data frames made by *Tracking Function* and
# puts them into a plottable format for ggplot
tp <- function(df){
  return(df %>% pivot_longer(cols = colnames(df)[c(-1,-2)],names_to = "Rate Type"))
}


#--Tracking Function--# 
# For any un/specified location 
# Requires source("pipe1.R") to be run
# Creates a day by day summary table for a given location
# returns a list( 1 data.frame with nrow = days, 1 plottable df)
#
track <- function(location = 'earth'){
  countryList <- unique(union(t()[["Country/Region"]],world[[1]][["Country/Region"]]))
  stateList <- unique(union(t()[["Province/State"]],world[[1]][["Province/State"]]))
  fipsList <- unique(t()[["FIPS"]])
  geopol <- NULL
  
  if(location %in% countryList){
    geopol <- "Country/Region"
  } 
  else if(location %in% stateList){
    geopol <- "Province/State"
  }
  else if(location %in% fipsList){
    geopol <- "FIPS"
    df <- data.frame()
    for(i in 61:length(world)){
      if(is_empty(df)){
        df <- world[[i]] %>% filter(grepl(location,world[[i]][[geopol]])) %>% 
          summarize(confirms = sum(Confirmed,na.rm = T),
                    deaths = sum(Deaths,na.rm = T),
                    recovers = sum(Recovered,na.rm = T)) %>% 
          mutate(active = confirms - deaths - recovers)
      } else{
        df <- df %>% rbind(world[[i]] %>% filter(grepl(location,world[[i]][[geopol]])) %>% 
                             summarize(confirms = sum(Confirmed,na.rm = T),
                                       deaths = sum(Deaths,na.rm = T),
                                       recovers = sum(Recovered,na.rm = T)) %>% 
                             mutate(active = confirms - deaths - recovers))
      }
    }
    
    df <- df %>% mutate(day = 1:nrow(df),
                        date = as.Date("2020-03-22") + (day-1)) %>% 
      select(day,date,confirms,deaths,recovers,active) %>% 
      addIncidence()
    return(df)
  }
  else if(location == 'earth'){
    df <- data.frame()
    for(i in world){
      if(is_empty(df)){
        df <- i %>% summarize(confirms = sum(Confirmed,na.rm = T),
                              deaths = sum(Deaths,na.rm = T),
                              recovers = sum(Recovered,na.rm = T)) %>% 
          mutate(active = confirms - deaths - recovers)
      } else {
        df <- df %>% rbind(i %>% summarize(confirms = sum(Confirmed,na.rm = T),
                                           deaths = sum(Deaths,na.rm = T),
                                           recovers = sum(Recovered,na.rm = T)) %>% 
                             mutate(active = confirms - deaths - recovers))
      }
    }
    
    df <- df %>% mutate(day = 1:nrow(df),
                        date = as.Date("2020-01-22") + (day-1) ) %>%
      select(day,date,confirms,deaths,recovers,active) %>% 
      addIncidence()
    return(df)
  } 
  else {
    print("No data on this location yet --OR-- Type the location differently")
    View(select(t(),FIPS,Combined_Key,`Province/State`,`Country/Region`))
    return(NULL)
  }
  
  # Looking for and returning a particular location
  df <- data.frame()
  for(i in world){
    if(is_empty(df)){
      df <- i %>% filter(grepl(location,i[[geopol]])) %>% 
        summarize(confirms = sum(Confirmed,na.rm = T),
                  deaths = sum(Deaths,na.rm = T),
                  recovers = sum(Recovered,na.rm = T)) %>% 
        mutate(active = confirms - deaths - recovers)
    } else{
      df <- df %>% rbind(i %>% filter(grepl(location,i[[geopol]])) %>% 
                           summarize(confirms = sum(Confirmed,na.rm = T),
                                     deaths = sum(Deaths,na.rm = T),
                                     recovers = sum(Recovered,na.rm = T)) %>% 
                           mutate(active = confirms - deaths - recovers))
    }
  }
  
  df <- df %>% mutate(day = 1:nrow(df),
                      date = as.Date("2020-01-22") + (day-1)) %>% 
    select(day,date,confirms,deaths,recovers,active) %>% 
    addIncidence()
  return(df)
}


#--Doubling Rate Calculator Function--#
# Calculates the doubling rate for a given
# location specificed with the 'c' or 's'
# databases made in this project
rdouble <- function(location,lastDays = length(world)){
  vec <- c()
  
  for(i in 1:nrow(location)){
    current <- location[['confirms']][i]
    doubled <- FALSE
    step <- 1
    while(!doubled){
      if(i+step >= nrow(location)){
        step <- NA
        break;
      } else if(location[['confirms']][i+step] >= 2*current){
        doubled <- TRUE
      } else {
        step <- step + 1
      }
    }
    vec[i] <- step
  }
  prev <- vec[(length(vec) - lastDays + 1):length(vec)]
  return(mean(prev,na.rm = T))
}










