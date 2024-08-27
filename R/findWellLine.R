#' read an excel file
#' 
#' read an excel file and promote the massage of the wells information
#' @param xls excel file of which the raw data are
#' @export


findWellLine<-function(xls) suppressMessages(which(as.data.frame(readxl::read_excel(xls,'Raw Data',n_max=50))[,1]=='Well'))