# Making predictions for how many cases there are in the US
source("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/pipe3.R")

# Libraries
library(ggplot2)

#look at some data
usts %>%  ggplot(mapping = aes(x = date, y = incidence)) +
  geom_line() 

# usts %>%  ggplot(mapping = aes(x = date, y = positive)) +
#   geom_line() + scale_x_date(name = "Date") +  
#   geom_abline(intercept = m$coefficients[[1]],
#               slope = m$coefficients[[2]],
#               color = 'red')


usts %>% ggplot(mapping = aes(x = date, y = (positive)/(positive+negative)))+
  geom_line()


usts2 <- usts %>% mutate(day = nrow(usts):1)

m <- lm(positive~day,data = usts2 %>% filter(date > "2020-03-30"))


f <- function(x){
  intoX <- yday(x) - yday("2020-02-28")
  #print(intoX)
  return(m$coefficients[[1]] + m$coefficients[[2]]*intoX)
}


invyday <- function(num){
  return(as.Date("2020-01-01") + num -1)
}











