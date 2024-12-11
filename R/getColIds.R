#' 
#' 

getRepeat<-function(IDs){
  needR<-readline(prompt = "Enter 1 to repeat by sequence(Ex: AA, BB, AA, BB); 2 by sample(Ex: AA, AA, BB, BB): ")
  loop1 <- needR == 1 || needR == 2
  while(!loop1){
    needR<-readline(prompt = "Please enter a vaild input. \nEnter 1 to repeat by sequence(Ex: AA, BB, AA, BB); 2 by sample(Ex: AA, AA, BB, BB): ")
    loop1 <- needR == 1 || needR == 2
  }
  if(needR==1){
    re<-readline(prompt = "Enter how many times you want to repeat: ")
    ID<-rep(IDs,re)
  }else{
    re<-readline(prompt = "Enter how many times you want to repeat: ")
    ID<-rep(IDs,each=re)
  }
  return(ID)
}

getColIds<-function(num){
  Input<-readline(prompt = "Enter the IDs (sperated by ',', Example: AA,BB,CC): ")
  ID<- unlist(strsplit(Input, ","))
  full<-getRepeat(ID)
  while(length(full)!=num){
    cat("ID not match.")
    Input<-readline(prompt = "Enter the IDs (sperated by ',', Example: AA,BB,CC): ")
    ID<- unlist(strsplit(Input, ","))
    full<-getRepeat(ID)
  }
  return(full)
}
