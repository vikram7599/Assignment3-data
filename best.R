best <- function(state, outcome){
  data <- read.csv('outcome-of-care-measures.csv' , colClasses = 'character')
  
  ans <- "set"
  data1 <- subset(data,data$State == state)
  if(outcome == "heart attack"){
    data1[,11] <- as.numeric(data1[,11])
    data2 <- subset(data1, !is.na(data1[,11]))
    data3 <- data2[order(data2$Hospital.Name),]
    data4 <- data3[order(data3$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
  }
  else if(outcome == "heart failure"){
    data1[,17] <- as.numeric(data1[,17])
    data2 <- subset(data1, !is.na(data1[,17]))
    data3 <- data2[order(data2$Hospital.Name),] 
    data4 <- data3[order(data3$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
  }
  else if(outcome == "pneumonia"){
    data1[,23] <- as.numeric(data1[,23])
    data2 <- subset(data1, !is.na(data1[,23]))
    data3 <- data2[order(data2$Hospital.Name),]
    data4 <- data3[order(data3$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
  }
  
  data4[1,2] 
  
}