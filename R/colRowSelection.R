#' 
#' No parameter needed
#' 
#' With no parameter, the function will promote the user to input the col information
#' 
#' @return a squeal that tells the range of the column
#' @export
#' @example 
#' tCol <- totalCol()
#' result <- colSelection(tCol)
#' selCol <- result$seqCol
#' numC <- result$num
#' 


colSelection <- function(numC = 12){
  print(numC)
  startFrom <- readline(prompt = "Selection the col want to start with (must be integer): ")
  startFrom <- as.integer(startFrom)
  endAt <- readline(prompt = "End the col reading at (must be integer and bigger or equal than starting col): ")
  endAt <- as.integer(endAt)
  inter <- readline(prompt = "Interval? (enter 1 if no need for interval)")
  inter <- as.integer(inter)
  inter = max(inter, 1)
  #uncomment line below for troubleshot
  #print(seq(startFrom,endAt, inter))
  while(startFrom < 1 || endAt > numC){
    cat("Error number, try again.\n")
    startFrom <- readline(prompt = "Selection the col want to start with (must be integer): ")
    startFrom <- as.integer(startFrom)
    endAt <- readline(prompt = "End the col reading at (must be integer and bigger or equal than starting col): ")
    endAt <- as.integer(endAt)
    inter <- readline(prompt = "Interval? (enter 1 if no need for interval)")
    inter <- as.integer(inter)
    inter = max(inter, 1)
  }
  seqCol <- seq(startFrom, endAt, inter)
  num = (endAt - startFrom + 1)/inter
  return(list(seqCol = seqCol, num = num))
}

#' 
#' No parameter needed
#' 
#' With no parameter, the function will promote the user to input the row information
#' @return a squeal that tells the range of the rows
#' @export
#' @example 
#' tRow <- totalRow()
#' result <- rowSelection(tRow)
#' selrow <- result$seqRow
#' numC <- result$num
#' 


rowSelection <- function(numR = 8){
  startFromr <- readline(prompt = "Selection the row want to start with (must be integer): ")
  startFromr <- as.integer(startFromr)
  endAtr <- readline(prompt = "End the row reading at (must be integer and bigger or equal than starting row): ")
  endAtr <- as.integer(endAtr)
  interR <- readline(prompt = "Interval? (enter 1 if no need for interval)")
  interR <- as.integer(interR)
  interR = max(interR, 1)
  #uncomment line below for troubleshot
  #print(seq(startFromr,endAtr, interR))
  while(startFromr < 1 || endAtr > numR){
    cat("Error number, try again.\n")
    startFromr <- readline(prompt = "Selection the col want to start with (must be integer): ")
    startFromr <- as.integer(startFromr)
    endAtr <- readline(prompt = "End the col reading at (must be integer and bigger or equal than starting col): ")
    endAtr <- as.integer(endAtr)
    interR <- readline(prompt = "Interval? (enter 1 if no need for interval)")
    interR <- as.integer(interR)
    interR = max(interR, 1)
  }
  seqRow <- seq(startFromr, endAtr, interR)  
  num = (endAtr - startFromr)/interR
  return(list(seqRow = seqRow, num = num))
}