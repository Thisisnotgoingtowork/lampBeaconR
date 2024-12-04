# setwd('C:/Users/jiang/Downloads/lampPackage')
# source('R')
# Set the path to your folder
folder_path <- 'C:/Users/jiang/Downloads/lampPackage/R'

# Get a list of all R script files in the folder
r_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)

# Source each file
lapply(r_files, source)
file1 <-ImportXlsx()
data=readXls(file1,file1,nCycle=300,isQS6=TRUE) 
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
file="R\10-23 CLPZ F_20240916_120422.xlsx"
