# Plotting the curve
source("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/pipe3.R")

library(dplyr)

usts %>% 
  mutate(COVID_related_Hospitalization = (hospitalizedCurrently/`Total Hospital Beds`),
         Available_Capacity = 1 - 0.5 - COVID_related_Hospitalization,
         Other_Capacity = ifelse(date > "2020-03-21",0.5,NA)) %>% tp %>% 
  filter(Rate_Type == 'COVID_related_Hospitalization'|Rate_Type == "Other_Capacity" | Rate_Type == 'Available_Capacity') %>%
  ggplot(aes(x=date,y = value))+
  geom_area(aes(fill = Rate_Type)) +
  labs(y = "Proportion of Hospital Beds",
       title = "The Curve for the US")


stts %>% filter(state == "NY") %>% 
  mutate(COVID_related_Hospitalization = (hospitalizedCurrently/`Total Hospital Beds`),
         Available_Capacity = 1 - 0.5 - COVID_related_Hospitalization,
         Other_Capacity = ifelse(date > "2020-03-25",0.5,NA)) %>% tp %>% 
  filter(Rate_Type == 'COVID_related_Hospitalization'|Rate_Type == "Other_Capacity" | Rate_Type == 'Available_Capacity') %>%
  ggplot(aes(x=date,y = value))+
  geom_area(aes(fill = Rate_Type)) +
  labs(y = "Proportion of Hospital Beds",
       title = "The Curve for NY")






