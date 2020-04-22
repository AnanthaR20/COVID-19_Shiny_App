# Making predictions for how many cases there are in the US
if(!exists('pipe3ran')){
  source("0)_setup/pipe3.R")
}
#Making Models
support <- 0:365
us <- 1000000#330415717

############US Population####################
#Model Predictions for US:
USpop <- rep(us,length(support))

m <- lm(positive ~ day , data = usts)
m1 <- lm(positive ~ day, data = (usts %>% filter(date > today()-7)))
m2 <- lm(positive ~ day, data = (usts %>% filter(date > today()-14)))
m3 <- lm(positive ~ day, data = (usts %>% filter(date > today()-21)))

f <- function(x){return(m$coefficients[[1]] + m$coefficients[[2]]*x)}
f1 <- function(x){return(m1$coefficients[[1]] + m1$coefficients[[2]]*x)}
f2 <- function(x){return(m2$coefficients[[1]] + m2$coefficients[[2]]*x)}
f3 <- function(x){return(m3$coefficients[[1]] + m3$coefficients[[2]]*x)}

USpredictions <- data.frame(matrix(nrow = length(support),ncol = 6))
USpredictions[[1]] <- support
USpredictions[[2]] <- USpop
USpredictions[[3]] <- f(support)
USpredictions[[4]] <- f1(support)
USpredictions[[5]] <- f2(support)
USpredictions[[6]] <- f3(support)
USpredictions[[7]] <- as.Date("2020-02-28") + (support)
colnames(USpredictions) <- c("day","1 Million Cases","timeline","last1Week","last2Weeks","last3Weeks",'date')
USpredictions <- USpredictions %>% pivot_longer(cols = c("1 Million Cases","timeline","last1Week","last2Weeks","last3Weeks"),names_to = "model")


################## Model prediciton for the US #################
####################plots of models#################################

USpredictions %>% ggplot(mapping = aes(x = date,color = model)) +
  geom_line(mapping = aes(y = value)) + labs(x = "Date", y = "# Confirmed Cases",
                                             title = "Projected trajectory fit over data from specified ranges") +
  xlim(as.Date("2020-03-01"),as.Date("2020-12-31"))











# 
# m <- lm(positive~day,data = usts2 %>% filter(date > "2020-03-30"))
# 
# 
# f <- function(x){
#   intoX <- yday(x) - yday("2020-02-28")
#   #print(intoX)
#   return(m$coefficients[[1]] + m$coefficients[[2]]*intoX)
# }
# 
# 
# invyday <- function(num){
#   return(as.Date("2020-01-01") + num -1)
# }
# 
# 
# usts %>%  ggplot(mapping = aes(x = date, y = positive)) +
#   geom_line() + scale_x_date(name = "Date") +
#   geom_abline(intercept = m$coefficients[[1]],
#               slope = m$coefficients[[2]],
#               color = 'red')
# 







