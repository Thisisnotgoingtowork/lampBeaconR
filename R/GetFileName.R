#' 
#' No parameter needed
#' 
#' With no parameter, the function will promote the user to input a file name that will later be used
#' @return a string
#' @export
#' 

GetFileName<-function(){
  valid_filename <- readline(prompt ="Please entre a name WITHOUT space (example: AAA): ")
  if (grepl(" ", valid_filename)) {
    stop("There is space in the name, please enter again")
  }
  final_name <- paste0(valid_filename, ".pdf")
  return(final_name)
}

#' 
#' No parameter needed
#' 
#' With no parameter, the function will promote the user to input a file name that will later be used
#' @return a string
#' @export
#' 
#' 
GetlogName <- function(){
  valid_filename <- readline(prompt ="Please entre a log file name WITHOUT space (example: AAA): ")
  if (grepl(" ", valid_filename)) {
    stop("There is space in the name, please enter again")
  }
  final_name <- paste0(valid_filename, ".txt")
  return(final_name)
}