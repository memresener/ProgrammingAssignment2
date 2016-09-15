best<-function(state,outcome){
  
  outcome.table<-c("pneumonia"=23,"heart attack"=11,"heart failure"=17)
  table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  if(!(state %in% table$State)){
    stop("invalid state")
  }
  else if(!(outcome %in% names(outcome.table))){
    stop("invalid outcome")
  }
  else{
    split.table<-subset(table,State==state, select = c(2,outcome.table[outcome]))
    suppressWarnings(split.table$Hospital.Name[order(as.numeric(split.table[,2]),split.table$Hospital.Name, decreasing = FALSE, na.last=NA)][1])
  }
}