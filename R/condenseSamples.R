#' 
#' Condense excel file data
#' 
#' Reading excel file and used it to output data frame
#' @param files An excel file that have the raw data of lamp
#' @param pattern48 an expression pattern that to match sheet name initial with '^48'
#' @param rows the selection of rows to read in the file, initial with 1:8
#' @param cols the selection of cols to read in the file, initial with 1:6
#' @return A data frame containing condensed sample data with the corresponding file and sheet names.
#' 
#' @export
#' 

condenseSamples<-function(files,pattern48='^48',rows=1:8,cols=1:6){
  out<-do.call(rbind,lapply(files,function(xx){
    sheets<-readxl::excel_sheets(xx)
    do.call(rbind,lapply(sheets[grepl(pattern48,sheets)],function(yy){
      tmp<-as.data.frame(suppressMessages(readxl::read_excel(xx,yy)))[rows,cols]
      out<-data.frame('row'=rep(1:length(rows),length(cols)),'col'=rep(1:length(cols),each=length(rows)),'sample'=unlist(tmp),stringsAsFactors=FALSE)
      out$file<-xx
      out$tab<-yy
      return(out)
    }))
  }))
  return(out)
}
