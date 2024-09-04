#' reading a list of data
#'
#' reading a list of data of cycling lamp data, and return a list with calculated amplitude
#' @param lamp a list of data that plot by the cycling data
#' @return a list of data with calculated amplitude
#' @export
#' 

calcAmps<-function(lamp){
  amped<-cbind(
    'nm520'=checkAmps(lamp$lamp,lamp$melt,'520nm',minFoldIncrease=1.5,meltTempNum=47,meltTempNum2=53,minMeltDiff=1.25,minAmp=10000,baselineTime=4),
    'nm587'=checkAmps(lamp$lamp,lamp$melt,'587nm',minFoldIncrease=1.5,meltTempNum=1,meltTempNum2=60,minMeltDiff=1.5,baselineTime=4),
    'nm623'=checkAmps(lamp$lamp,lamp$melt,'623nm',minFoldIncrease=2,meltTempNum=38,meltTempNum2=50,minMeltDiff=1.2,minAmp=10000,baselineTime=4),
    'nm682'=checkAmps(lamp$lamp,lamp$melt,'682nm',minFoldIncrease=2,minAmp=20000,meltTempNum=1,meltTempNum2=65,minMeltDiff=2,baselineTime=4)
  )
  amped$target<-sapply(rownames(amped),function(xx)lamp$lamp$target[lamp$lamp$Well==xx][1])
  pos<-data.frame(
    'STATH'=tapply(amped$nm587.isGood,amped[,c('target')],sum),
    'As1e'=tapply(amped$nm623.isGood,amped[,c('target')],sum),
    'Penn'=tapply(amped$nm682.isGood,amped[,c('target')],sum),
    'E1'=tapply(amped$nm520.isGood,amped[,c('target')],sum)
  )
  return(list('amped'=amped,'pos'=pos))
}