#-------------------------------------------------------------#
#-------------------------------------------------------------#
# Pipe line for in-depth US data on COVID-19
#-------------------------------------------------------------#
#-------------------------------------------------------------#
# ===0=== #
source("0)_setup/library.R")
# ===1=== #
# Manually made key for US state names and abbreviatsions
uskey <- read.csv("0)_setup/uskey.csv")
#-------------------------------------------------------------#
# ===2=== #
# https://www.cnn.com/2020/03/23/us/coronavirus-which-states-stay-at-home-order-trnd/index.html
# Constructed a chart from 'investigative' scraping from CNN website
# Data.frame created manually on 2020-04-21 at 7:30pm
stay_home <- read.csv("0)_setup/StayHomeIssueDates.csv")
stay_home$state <- as.character(stay_home$state)
stay_home$date_StayHomeOrder_issued <- as.Date(stay_home$date_StayHomeOrder_issued)
#-------------------------------------------------------------#
# ===3=== #
#KFF data: https://www.kff.org/health-costs/issue-brief/state-data-and-policy-actions-to-address-coronavirus/
cap <- fread("0)_setup/KFF/HealthCapacity.csv",fill = T)

# pol <- fread("0)_setup/KFF/PublicPolicy.csv",fill = T)
#-------------------------------------------------------------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#
# ===4=== #
# The followig data comes from https://covidtracking.com/api which collects data

# US aggregate time series data on several variables
usts <- fread("https://covidtracking.com/api/us/daily.csv") %>% 
  select(-13,-14,-16:-25)
usts$date <- as.Date(sapply(as.character(usts$date),function(char){
  yr <- char %>% str_sub(1,4)
  m <- char %>% str_sub(5,6)
  day <- char %>% str_sub(-2,-1)
  return((str_c(yr,"-",m,"-",day)))
}))

#add days
usts <- usts %>% mutate(day = nrow(usts):1) %>% select(day,grep(".",colnames(usts)))

#add cap info
usts$Location <- "United States"
usts <- usts %>% left_join(cap, by = "Location")

usts <- usts %>% mutate(incidence = positive-lead(positive), 
                R0 = incidence/lead(incidence))

#__________________________________________________

# States time series data on several variables
stts <- fread("https://covidtracking.com/api/v1/states/daily.csv") %>% 
  select(-13,-14,-16:-25)
stts$date <- as.Date(sapply(as.character(stts$date),function(char){
  yr <- char %>% str_sub(1,4)
  m <- char %>% str_sub(5,6)
  day <- char %>% str_sub(-2,-1)
  return((str_c(yr,"-",m,"-",day)))
}))

#add state names and cap data
stts <- stts %>% left_join(uskey[-3],by = c("state" = "abb")) %>% 
  left_join(cap, by = c("state.y" = "Location"))

# add stay-at-home order data
stts <- stts %>% left_join(stay_home[-1],by = c("state" = "abb"))

#__________________________________________________

# A data frame about testing in the US
cdc <- fread("https://covidtracking.com/api/cdc/daily.csv") %>% 
  mutate(cumulative = cumsum(dailyTotal)*(1-lag))
#-------------------------------------------------------------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#
# ===5=== #
# Run functions for pipe3 data
source("0)_setup/pipe3functions.R")


pipe3ran <- TRUE

#Examine difference between this account and the john hopkins data
# rev(filter(track("Wisconsin"),date >= "2020-03-04")[['confirms']]) - 
#   (filter(stts,state =='WI')[['positive']]) -> a
