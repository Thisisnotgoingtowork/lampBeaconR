


ImportXlsx<-function(){
  loop<- TRUE
  filename<-readline(prompt = "Enter the excel file to analyze(must be .xlsx file): ")
  while(loop){
    if (grepl(".xlsx", filename)){
      loop<-FALSE
    }
    else{
      filename<-readline(prompt = "Error file. \nEnter the excel file to analyze(must be .xlsx file): ")
    }
  }
  return(filename)
}