# Doubling Rate before and after the stay at home per state
if(!exists('pipe3ran')){
  source("0)_setup/pipe3.R")
}


subtract <- yday(min(track2("NY")[['date']]))

day1 <- yday(min(track2("NY")[['date']])) - subtract + 1
day2 <- yday(stay_home$date_StayHomeOrder_issued[stay_home$state == "New York"]) - subtract
day3 <- today() - subtract


rdouble2(track2("NY"),day1,day2)
rdouble2(track2("NY"),day2+1,day3)






rdouble2 <- function(df,from =1,to = nrow(df)){
  counts <- c()
  for(i in 1:nrow(df)){
    current <- rev(df[['positive']])[i]
    nextVal <- rev(df[['positive']])[i+1]
    count <- 0
    while(T){
      if(i+1+count > nrow(df)){
        count <- NA
        break;
      } else if(nextVal < 2*current){
        count <- count + 1
        nextVal <- rev(df[['positive']])[i+1+count]
      } else if(nextVal >= 2*current){
        count <- count + 1
        break;
      } else {
        break;
      }
    }
    counts[i] <- count
  }
  
  return(mean(counts[from:to],na.rm = T))
  
}












