# Summon all libraries + necessary scripts
library(tidyverse)
library(data.table)
library(lubridate)


readUrl <- function(url) {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      #message("This is the 'try' part")
      
      readLines(con=url, warn=FALSE) 
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      message(cond)
      message("
              Error: Data is not available for some requested date.")
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      message("
              Data is not available for some requested date.")
      
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message(paste("Processed URL:", url))
      #message("Some other message at the end")
    }
  )    
  return(out)
}




# 
# uskey <- data.frame(matrix(nrow = 56,ncol=2))
# uskey[[1]] <- unique(stts$state)
# colnames(uskey) <- c("abb","state")
# uskey[[2]] <- n
# 
# uskey <- uskey %>% filter(state != "")
# 
# alph <- order(uskey$state)
# uskey$abb <- uskey$abb[alph]
# uskey$state <- uskey$state[alph]
# uskey$fips <- fips
# 
# 
# View(uskey)
# 
# 
# k <- read.csv("uskey.csv")

