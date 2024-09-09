#' 
#' No parameter needed
#' 
#' With no parameter, the function will promote the user to input the col information
#' @return a squeal
#' @export
#' 


colSelection <- function(){
  startFrom <- readline(prompt = "Selection the col want to start with (must be integer): ")
  startFrom <- as.integer(startFrom)
  endAt <- readline(prompt = "End the col reading at (must be integer and bigger or equal than starting col): ")
  endAt <- as.integer(endAt)
  inter <- readline(prompt = "Interval? (enter 1 if no need for interval)")
  inter <- as.integer(inter)
  inter = max(inter, 1)
  #uncomment line below for troubleshot
  #print(seq(startFrom,endAt, inter))
  return(seq(startFrom, endAt, inter))
}


#' 
#' No parameter needed
#' 
#' With no parameter, the function will promote the user to input the row information
#' @return a squeal
#' @export
#' 


rowSelection <- function(){
  startFromr <- readline(prompt = "Selection the row want to start with (must be integer): ")
  startFromr <- as.integer(startFromr)
  endAtr <- readline(prompt = "End the row reading at (must be integer and bigger or equal than starting row): ")
  endAtr <- as.integer(endAtr)
  interR <- readline(prompt = "Interval? (enter 1 if no need for interval)")
  interR <- as.integer(interR)
  interR = max(interR, 1)
  #uncomment line below for troubleshot
  #print(seq(startFromr,endAtr, interR))
  return(seq(startFromr, endAtr, interR))
}