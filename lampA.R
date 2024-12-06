# setwd('C:/Users/jiang/Downloads/lampPackage')
# source('R')
# Set the path to your folder
folder_path <- 'C:/Users/jiang/Downloads/lampPackage/R'

# Get a list of all R script files in the folder
r_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)

# Source each file
lapply(r_files, source)
file1 <-ImportXlsx()
run<-overAllColBase(file1)
file="R\10-23 CLPZ F_20240916_120422.xlsx"
