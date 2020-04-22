#
if(!exists('pipe3ran')){
  source("setup/pipe3.R")
}



usts %>% ggplot(aes(x=date,y=positive))+
  geom_line() + labs(y="# Confirmed Cases", title = "# Confirmed Cases of COVID-19 in the US")

usts %>% ggplot(aes(x=positive, y = incidence))+
  geom_line()+
  labs(x= '#Confirmed Cases',y="Number of New Cases in a day")

