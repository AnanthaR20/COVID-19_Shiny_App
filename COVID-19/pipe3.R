# This pipeline comes from https://covidtracking.com/api which collects data
# from all of these sources.

library(data.table)
library(dplyr)
library(stringr)

# Run functions for pipe3 data

source("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/pipe3functions.R")

#KFF data
cap <- fread("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/HealthCapacity.csv",fill = T)

#pol <- fread("setup/KFF/PublicPolicy.csv",fill = T)

#-------------------------------------------------------------#
# US aggregate time series data on several variables
usts <- fread("https://covidtracking.com/api/us/daily.csv") %>% 
  select(-13,-14,-16:-24)
usts$date <- as.Date(sapply(as.character(usts$date),function(char){
  yr <- char %>% str_sub(1,4)
  m <- char %>% str_sub(5,6)
  day <- char %>% str_sub(-2,-1)
  return((str_c(yr,"-",m,"-",day)))
}))

usts$Location <- "United States"
usts <- usts %>% left_join(cap, by = "Location")

usts <- usts %>% mutate(incidence = positive-lead(positive), 
                R0 = incidence/lead(incidence))

#-------------------------------------------------------------#
# States time series data on several variables
stts <- fread("https://covidtracking.com/api/v1/states/daily.csv") %>% 
  select(-13,-14,-16:-25)
stts$date <- as.Date(sapply(as.character(stts$date),function(char){
  yr <- char %>% str_sub(1,4)
  m <- char %>% str_sub(5,6)
  day <- char %>% str_sub(-2,-1)
  return((str_c(yr,"-",m,"-",day)))
}))


stts <- stts %>% left_join(uskey[-3],by = c("state" = 'abb')) %>% 
  left_join(cap, by = c("state.y" = "Location"))

stts <- stts %>% mutate(incidence = positive-lead(positive), 
                        R0 = incidence/lead(incidence))
#-------------------------------------------------------------#

# A data frame about testing in the US
cdc <- fread("https://covidtracking.com/api/cdc/daily.csv") %>% 
  mutate(cumulative = cumsum(dailyTotal)*(1-lag))
#-------------------------------------------------------------#


#Examine difference between this account and the john hopkins data
# rev(filter(track("Wisconsin"),date >= "2020-03-04")[['confirms']]) - 
#   (filter(stts,state =='WI')[['positive']]) -> a
