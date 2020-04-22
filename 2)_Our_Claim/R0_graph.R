# R0 graph with respect to the stay at home order date per state
if(!exists('pipe3ran')){
  source("0)_setup/pipe3.R")
}
# Must be capitalized and spelled correctly
plotState <- "Wisconsin"

track2(plotState) %>% ggplot(aes(x=date,y=R0))+
  geom_line() +
  geom_vline(xintercept = 
            stay_home$date_StayHomeOrder_issued[stay_home$state == plotState],
            color = 'red' ) +
  labs(x = "Date", 
       y = "Ratio of incidence today to yesterday",
       title = "Examining the acceleration factor of infection in WI")
