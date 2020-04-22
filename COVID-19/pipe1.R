# :::::::::::::::::::::::::::::::::::::::::::::: #
# Pipe in the Data from Johns Hopkins Repository #
# :::::::::::::::::::::::::::::::::::::::::::::: #
# This script creates a 'heat' list of the full available
#   timeline of the epidemic. It is the list that shows the
#   time 'under heat' so it is named 'world' and contains all the data
# At the end of this script there are a couple of function associated
#   with the way the data is structured to get useful info from it.
source("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/library.R")

world <- list()

# Create
yrstart <- as.Date("2020-01-01")
dataperiod1 <- yday("2020-01-22"):yday(now())
dataperiod1 <- yrstart + (dataperiod1-1)
dataperiod1 <- format(dataperiod1,'%m-%d-%Y')

count <- 1
#Bring in all the data up until Today
for(d in dataperiod1){
  url <- str_c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",d,".csv")
  if( is.na(readUrl(url)) || is.null(readUrl(url)) ){
    
  } else {
    print(url)
    world[[count]] <- fread(url)
    count <- count + 1
  }
}
# Make Country+Region and Province+State and Last_Update colnames consistent
for(i in 1:length(world)){
  if("Province_State" %in% colnames(world[[i]])){
    colnames(world[[i]])[colnames(world[[i]]) == "Province_State"] <- "Province/State"
  }
  if("Country_Region" %in% colnames(world[[i]])){
    colnames(world[[i]])[colnames(world[[i]]) == "Country_Region"] <- "Country/Region"
  }
  if("Last Update" %in% colnames(world[[i]])){
    colnames(world[[i]])[colnames(world[[i]]) == "Last Update"] <- "Last_Update"
  }
}
# Handle inconsistent naming convention for South Korea
for(i in 1:length(world)){
    world[[i]]$`Country/Region` <- ifelse((world[[i]]$`Country/Region` == "Korea, South" | world[[i]]$`Country/Region` == "Republic of Korea"),"South Korea",world[[i]]$`Country/Region`)
}

# # Add longitude and latitude to the whole data set
# lon_lat_key <- world[[length(world)]] %>% select(`Country/Region`,`Province/State`,Longitude,Latitude)
# 
# for(i in 1:length(world)){
#   if(is.null(world[[i]][["Longitude"]])){
#     world[[i]] <- world[[i]] %>% left_join(lon_lat_key,by = c("Country/Region","Province/State"))
#   }
# }

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# -- Functions for the 'heat' 'h' list -- #

source("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/pipe1functions.R")
