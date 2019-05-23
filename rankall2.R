
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", 
                           colClasses = "character")
  
  ## Check that state is valid
  if (!is.element(state, outcome_data$State)) {
    stop("invalid state")
  }
  
  ## Check that outcome is valid
  possibleOutcomes = c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, possibleOutcomes)) {
    stop("invalid outcome")
  }
  
  ## Determine data that corresponds to the outcome
  if (outcome == "heart attack") {
    indexval <- 11
  } else if (outcome == "heart failure") {
    indexval <- 17
  } else if (outcome == "pneumonia") {
    indexval <- 23
  }    
  
  ## Generate list with a data frame for each state
  listbystate <- split(outcome_data, outcome_data$State)
  
  ## Generate data frame for state of interest only
  state_data <- listbystate[[state]]
  
  ## Substitute row with %death rate of outcome to numeric values
  subrow <- gsub("Not Available", NA, state_data[, indexval])
  state_data[, indexval] <- as.numeric(subrow)
  
  ## Re-order the state_data 
  ### first by hospital name in ascending order 
  ### then by death rate outcome in ascending order
  orderbyname <- state_data[order(state_data$Hospital.Name),]
  orderedstatedata <- orderbyname[order(orderbyname[,indexval], na.last = NA)
                                  ,]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  if (is.numeric(num) == TRUE) {
    rankedhospital <- orderedstatedata$Hospital.Name[num]
  } else if (num == "best") {
    rankedhospital <- orderedstatedata$Hospital.Name[1]
  } else if (num == "worst") {
    rankedhospital <- orderedstatedata$Hospital.Name[dim(orderedstatedata)
                                                     [1]] 
  }
  
  rankedhospital
  
}