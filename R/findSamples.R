#' 
#' Finding sample from an excel file
#' 
#' Using sample name to find matching samples in an excel file that content raw data 
#' and return organized sample information in list
#' 
#' @param samples A string that indicate the samples name
#' @param files An excel file that content lamp raw data
#' @param outFile a


findSamples<-function(samples,files,outFile=NULL){
  if(length(samples)==1 && file.exists(samples))samples<-read.csv(samples,stringsAsFactors=FALSE)[,1]
  dat<-condenseSamples(files)
  ids<-lapply(samples,function(xx)which(dat$sample==xx))
  nHit<-sapply(ids,length)
  if(any(nHit==0))warning('Samples ',paste(samples[nHit==0],collapse=', '),' not found')
  if(any(nHit>1))warning('Samples ',paste(samples[nHit>1],collapse=', '),' had multiple matches')
  filler<-dat[1,]
  filler[,]<-NA
  matches<-do.call(rbind,lapply(1:length(samples),function(ii){
    xx<-ids[[ii]]
    if(length(xx)==0)out<-filler
    else out<-dat[xx,]
    if(any(!out$sample %in% c(samples[ii],NA)))stop('Problem with sample finding')
    out$sample<-samples[ii] #fill in for missing samples
    return(out[,c('sample','file','tab','row','col')])
  }))
  if(!is.null(outFile))write.csv(matches,outFile,row.names=FALSE)
  return(matches)
}
