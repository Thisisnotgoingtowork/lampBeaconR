#' read excel file
#' 
#' reading excel file and return with formatted lamp data list
#' @param xls an excel file that have the raw data
#' @param isQS6 a Boolean value indicate if the data come from QuantStudio 6
#' @param extraTemps description ***
#' @param nCycle a int number indicate how many cycle had in the data
#' @return a formatted list **
#' @export

readXls<-function(xls,isQS6=FALSE,extraTemps=c(),nCycle=200){
  if(dir.exists(xls)){
    file1<-list.files(xls,'_Raw Data_',full.names=TRUE)
    file2<-list.files(xls,'_Melt Curve Raw_',full.names=TRUE)
    file3<-list.files(xls,'_Melt Curve Result',full.names=TRUE)
    lamp<-readLamp(file1,file2,nCycle+1,skip=23,correctCycles=TRUE)
    info<-unique(read.csv(file3,stringsAsFactors=FALSE,skip=23)[,c('Well.Position','Sample')])
    info[,'Sample Name']<-trimws(info$Sample)
    rownames(info)<-info[,'Well.Position']
  }else if(isQS6){
    nSkip<-findWellLine(xls)-1
    lamp<-readLamp(xls,xls,nCycle+1,skip=nSkip,correctCycles=TRUE,meltRawTab='Melt Curve Raw')
    info<-unique(as.data.frame(readxl::read_excel(xls,'Results',skip=nSkip))[,c('Well Position','Sample')])
    info[,'Sample Name']<-trimws(info$Sample)
    rownames(info)<-info[,'Well Position']
  }else{
    nSkip<-findWellLine(xls)-1
    lamp<-readLamp(xls,xls,nCycle+1,skip=nSkip,meltRawTab='Melt Curve Raw Data')
    info<-unique(as.data.frame(readxl::read_excel(xls,'Results',skip=nSkip))[,c('Well Position','Sample Name')])
    rownames(info)<-info[,'Well Position']
  }
  lamp<-lapply(lamp[sapply(lamp,function(xx)!is.null(xx)&&nrow(xx)>0)],function(xx){if(is.null(xx))return(xx);xx$target<-info[match(xx$well,rownames(info)),'Sample Name'];xx$dummy<-1;return(xx[order(xx$target),])})
  if(!is.null(extraTemps)&&!is.null(lamp$extra)&&nrow(lamp$extra)>0){
    lamp$extra$temp<-extraTemps[lamp$extra$Cycle-nCycle]
    if(is.null(lamp$melt))lamp$melt<-lamp$extra
  }
  #lamp<-lapply(lamp,function(xx){xx[!is.na(xx$target),]})
  return(lamp)
}