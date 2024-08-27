#' read list of data from readStepone
#' 
#' yhid read the data list and to check the amplitude from the previous functions
#' 
#' @param lamp a list of data provided by lamp file
#' @param melt a list of data provided by melt file
#' @param minAmp an int number indicate the minimum amplitude, initial with 2500mn
#' @param meltTempNum an int number indicate the melting temperature, initial with 35 degrees
#' @param meltTempNum2 an int number **
#' @param minFoldIncrease an int number, initial with 3
#' @param minMeltDiff a number that indicate the minimum difference between each melt data, initial with 0.5
#' @param baselineTime a number that indicate the staring base time is
#' @param isTemp a Boolean value **
#' @export
#' 



checkAmps<-function(lamp,melt,fl,minAmp=2500,meltTempNum=35,meltTempNum2=length(unique(melt$temp)),minFoldIncrease=3,minMeltDiff=.5,baselineTime=1,isTemp=FALSE){
  last<-dnar::withAs(xx=lamp[order(lamp$Well,lamp$time),],tapply(xx[,fl],list(xx$Well),function(zz)tail(zz,1)))
  first<-dnar::withAs(xx=lamp[order(lamp$Well,lamp$time),],tapply(xx[,fl],list(xx$Well),function(zz)max(1000,head(zz,1))))
  if(isTemp){
    melt$t1<-ave(melt$temp,melt$Well,FUN=function(xx)xx[which.min(abs(xx-meltTempNum))])
    melt$t2<-ave(melt$temp,melt$Well,FUN=function(xx)xx[which.min(abs(xx-meltTempNum2))])
    fl1<-tapply(melt[melt$temp==melt$t1,fl],melt[melt$temp==melt$t1,'Well'],c)
    fl2<-tapply(melt[melt$temp==melt$t2,fl],melt[melt$temp==melt$t1,'Well'],c)
  }else{
    fl1<-dnar::withAs(xx=melt[order(melt$Well,melt$temp),],tapply(xx[,fl],xx$Well,function(zz)zz[meltTempNum]))
    fl2<-dnar::withAs(xx=melt[order(melt$Well,melt$temp),],tapply(xx[,fl],xx$Well,function(zz)zz[meltTempNum2]))
  }
  curveMelt<-fl1>pmax(minAmp,fl2*minMeltDiff)
  lampAmp<-dnar::withAs(xx=lamp[order(lamp$Well,lamp$time),],tapply(xx[,fl],list(xx$Well),function(zz)tail(zz,1)>max(minAmp,zz[baselineTime]*minFoldIncrease)))
  out<-data.frame('amp'=lampAmp,'melt'=curveMelt[names(lampAmp)],'last'=last[names(lampAmp)],'first'=first[names(lampAmp)],row.names=names(lampAmp))
  out$isGood<-out$melt&out$amp
  out
}
