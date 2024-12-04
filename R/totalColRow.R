

totalCol<-function(){
  total <- readline(prompt = "Enter the total colume you have (from 1 to 12, min = 1, max  = 12): ")
  total <- as.integer(total)
  while (total < 1 || total > 12){
    print("Error value, try again.")
    total <- readline(prompt = "Enter the total colume you have (from 1 to 12, min = 1, max  = 12): ")
    total <- as.integer(total)
  }
  return(total)
}

totalRow<-function(){
  total <- readline(prompt = "Enter the total Row you have (from 1 to 8, min = 1, max  = 8): ")
  total <- as.integer(total)
  while (total < 1 || total > 8){
    print("Error value, try again.")
    total <- readline(prompt = "Enter the total Row you have (from 1 to 8, min = 1, max  = 8): ")
    total <- as.integer(total)
  }
  return(total)
}