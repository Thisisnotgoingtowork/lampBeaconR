#' reading a list of data
#'
#' reading a list of data of cycling lamp data, and return a list with calculated amplitude
#' the backup plan from calcAmps, delet the nm520 and specific the rows and cols etc.
#' @param lamp a list of data that plot by the cycling data
#' @return a list of data with calculated amplitude
#' @export
#' 
#' 

calcAmps2<-function(lamp){
  amped<-cbind(
    'nm587'=checkAmps(lamp$lamp,lamp$melt,'587nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=85,minMeltDiff=2,minAmp=20000,baselineTime=4,isTemp=TRUE),
    'nm682'=checkAmps(lamp$lamp,lamp$melt,'682nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=75,minMeltDiff=2,minAmp=100000,baselineTime=4,isTemp=TRUE),
    'nm520'=checkAmps(lamp$lamp,lamp$melt,'520nm',minFoldIncrease=3,minAmp=100000,meltTempNum=25,meltTempNum2=90,minMeltDiff=1.5,baselineTime=4,isTemp=TRUE)
  )
  amped$target<-sapply(rownames(amped),function(xx)lamp$lamp$target[lamp$lamp$Well==xx][1])
  amped$row<-sapply(rownames(amped),function(xx)lamp$lamp$row[lamp$lamp$Well==xx][1])
  amped$col<-sapply(rownames(amped),function(xx)lamp$lamp$col[lamp$lamp$Well==xx][1])
  if('enzyme' %in% colnames(lamp$lamp))amped$enzyme<-sapply(rownames(amped),function(xx)lamp$lamp$enzyme[lamp$lamp$Well==xx][1])
  pos<-data.frame(
    'STATH'=tapply(amped$nm587.isGood,amped[,c('target')],sum),
    'As1e'=tapply(amped$nm682.isGood,amped[,c('target')],sum),
    'Penn'=tapply(amped$nm520.isGood,amped[,c('target')],sum)
  )
  return(list('amped'=amped,'pos'=pos))
}