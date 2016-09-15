rankall<-function(outcome,num="best"){
  
  outcome.table<-c("pneumonia"=23,"heart attack"=11,"heart failure"=17)
  table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  num<-ifelse(num=="best",1,num)
  
  
  if(!(outcome %in% names(outcome.table))){
    
    stop("invalid outcome")
  }
  
  table<-table[order(as.numeric(table[,outcome.table[outcome]])),c(2,outcome.table[outcome],7)]
  names(table)<-c("Hospital Name", outcome,"State")
  na.omit(table)
  table<-split(table, table$State)
  
  if(is.numeric(num)){
    return<-lapply(table,function(x) x[num,-2])
                          
  }
  else if(num=="worst"){
    suppressWarnings(tail
                     ((split.table$Hospital.Name[order(as.numeric(split.table[,2]),
                                                       split.table$Hospital.Name, decreasing = FALSE, na.last=NA)]),n=1L))
  }
}