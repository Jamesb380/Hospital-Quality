##James Bennett
##Hospital Quality 
##Function to displace the best hospital 30 day mortality in a specified state for a specific cause


best <- function(state, outcome){
  
        df <- read.csv("outcome-of-care-measures.csv", header = T, colClasses = "character")
        
      if (confirm(state, outcome) != TRUE)
              break
        else    
                picker(df, state, outcome)
                 
        
        
         
         
}

confirm <- function (state, outcome) { 
        
        conditions <- c("heart attack", "heart failure", "pneumonia")
        cause <- outcome %in% conditions
        location <- state %in% state.abb
        
        if (cause != TRUE)
                print("not a valid condtion")
        
        
        if (location != TRUE)
                print("error")
        
        return(cause && location)
        }

picker <- function(df, state, outcome){
             
                
        s <- which(df$State == state) 
        trows <- df[s, ]
      
        position <- selectposition(outcome)
        
        b <-   trows[
        order (trows[,c(position,2)]),
                    ]
     
        b[1,c(2,position)]

}

selectposition <- function(deathcaused){
        
        if (deathcaused == "heart attack")
                position <- 11
        
        if (deathcaused == "heart failure")
               position <- 17
        
        if (deathcaused == "pneumonia")
                position <- 23
        
        position
        }

