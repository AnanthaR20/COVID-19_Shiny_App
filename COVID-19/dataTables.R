# Summary Tables #
source("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/library.R")
source("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/pipe1.R")
print("Running dataTables.R")
#--Summary Tables--#
# On Earth
earth <- track()

earthRecent <- earth[nrow(earth),-ncol(earth)]
colnames(earthRecent)[1] <- "Country/Region"
earthRecent[1,1] <- "---Worldwide Total---"

over1000 <- (t(0) %>% group_by(`Country/Region`) %>% 
  summarize(confirms = sum(Confirmed,na.rm = T)) %>% 
    filter(confirms > 1000))[["Country/Region"]]

allCountries <- unique((t(0) %>% group_by(`Country/Region`))[["Country/Region"]])

USStates <- unique((t(0) %>% filter(`Country/Region` == "US") %>% group_by(`Province/State`))[["Province/State"]])

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

c <- list()
for(country in allCountries){
  c[[country]] <- track(country)
}
s <- list()
for(state in USStates){
  s[[state]] <- track(state)
}

# Not China 
notChina <- earth - track("China")
notChina$day <- 1:nrow(notChina)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #




