

GetFileName<-function(){
  valid_filename <- readline(prompt ="Please entre a name WITHOUT space (example: AAA): ")
  if (grepl(" ", valid_filename)) {
    stop("There is space in the name, please enter again")
  }
  final_name <- paste0(valid_filename, ".pdf")
  return(final_name)
}