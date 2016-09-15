rankhospital<-function(state,outcome,num="best"){
  
  outcome.table<-c("pneumonia"=23,"heart attack"=11,"heart failure"=17)
  table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  num<-ifelse(num=="best",1,num)
  
  if(!(state %in% table$State)){
    
    stop("invalid state")
  }
  else if(!(outcome %in% names(outcome.table))){
    
    stop("invalid outcome")
  }
  
  split.table<-subset(table,State==state, select = c(2,outcome.table[outcome]))
  na.omit(split.table)
  
  if(is.numeric(num)){
    suppressWarnings(split.table$Hospital.Name[order(as.numeric(split.table[,2]),
                     split.table$Hospital.Name, decreasing = FALSE, na.last=NA)][num])
    
  }
  else if(num=="worst"){
    suppressWarnings(tail
                     ((split.table$Hospital.Name[order(as.numeric(split.table[,2]),
                     split.table$Hospital.Name, decreasing = FALSE, na.last=NA)]),n=1L))
  }
}