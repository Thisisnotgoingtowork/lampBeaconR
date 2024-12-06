#' 
#' A xlsx file containing lamp cycle raw data
#' 
#' This is an overall funtion that call out others to output the pdf files and a log file (recording the )
#' @return a squeal that tells the range of the column
#' @export
#' @example 
#' 
#' 


overAllColBase<-function(file1){
  nCycle<-readline(prompt = "Total Cycle? the number can be larger than the actual:  ")
  nCycle<- as.integer(nCycle)
  boolQ3<-readline(prompt = "Is the data from Quantan Studio 3? (y/n) ")
  while (boolQ3 != "y" && boolQ3 != 'n'){
    print("Only input y or n.\n")
    boolQ3<-readline(prompt = "Is the data from Quantan Studio 3? (y/n) ")
  }
  if (boolQ3 == 'y'){
  data=readXls(file1,file1,nCycle,isQS6=TRUE) }
  else{
    data=readXls(file1,file1,nCycle,isQS6=FALSE) 
  }
  tCol <- totalCol()
  tRow <- totalRow()
  logFile <- GetlogName()
  sink(logFile, append = TRUE)
  cat("Xlsx file that is used: \n",file1,"\n")
  cat("Total column: ", tCol, "\nTotal Row: ", tRow)
  sink()
  IDs<-getColIds(tCol)
  sink(logFile, append = TRUE)
  cat("\nIDs: ")
  cat(IDs,"\n")
  sink()
  result <- colSelection(tCol)
  selCol <- result$seqCol
  numC <- result$num
  
  result <- rowSelection(tRow)
  selRow <- result$seqRow
  numR <- result$num
    
  sink(logFile, append = TRUE)
  cat("Column to print: \n", selCol)
  cat("\nRow to print: \n", selRow)
  sink()
  name <-GetFileName()
  sink(logFile, append = TRUE)
  cat("\nFile for pdf1: ",name,"\n")
  sink()
  x <- lampDatapdf(name, data, selCol,selRow)
  name2 <-GetFileName()
  sink(logFile, append = TRUE)
  cat("\nFile for pdf1: ",name2,"\n")
  sink()
  y <- lampDataGraphpdf(name2,selCol, selRow, IDs)
}
