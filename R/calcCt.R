#' Calculate the cycle threshold (Ct) values for qPCR data
#'
#' This function calculates the Ct values for quantitative PCR (qPCR) data based on fluorescence readings.
#' @param fluor a vector that indicate the fluorescence data
#' @param well a vector that indicate the well identifiers
#' @param meta An optional data frame content metadata for each well
#' @param threshold An int that indicate the threshold of the fluorescence, initial with 100000
#' @param timePerStep A double that indicate the time take per step, initial with 0.5
#' @param digits An int that indicate the number of decimal of the calculated threshold round up to, initial with 1
#' @param maxTime An int that indicate the maximum time for each wells to reach to threshold, initial with 60\
#' @return A data frame that content Ct information and wells identifiers etc.
#' @export
#' 
#' 
calcCt<-function(fluor,well,meta=NULL,threshold=100000,timePerStep=.5,digits=1,maxTime=60){
  ct<-tapply(fluor,well,function(xx)suppressWarnings(approx(xx,1:length(xx),threshold)$y)*timePerStep)
  ct<-data.frame('ct'=as.vector(ct),'well'=names(ct),stringsAsFactors=FALSE)
  ct$row<-sub('^([A-Z]).*','\\1',ct$well)
  ct$col<-as.numeric(sub('^([A-Z])(.*)','\\2',ct$well))
  ct$rowNum<-structure(1:26,.Names=LETTERS)[ct$row]
  #out<-tapply(ct$ct,ct[,c('row','col')],round,1)
  ct[is.na(ct$ct),'ct']<-maxTime
  ct$ct<-round(ct$ct,digits=digits)
  if(!is.null(meta)){
    compressMeta<-by(meta,well,unique)
    if(any(sapply(compressMeta,nrow)>1))stop('More than one unique metadata for well ',paste(names(compressMeta)[sapply(compressMeta,nrow)>1],collapse=', '))
    ct<-cbind(ct,do.call(rbind,compressMeta)[ct$well,])
  }
  return(ct)
}