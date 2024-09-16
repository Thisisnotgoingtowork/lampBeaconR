#' 
#' reading a list
#' 
#' reading a list and return a list that content the calculated amplitude in generic version
#' @param lamp a list that have the information of the lamp
#' @param ... any additional parameter that needed to put in
#' @return A list containing the amplification results and a summary of positive amplifications.
#' @export

calcAmpsGeneric<-function(lamp,...){
  args<-list(...)
  if(length(args)==0)args<-list(
    'E1'=list(fl='520nm',minFoldIncrease=1.5,meltTempNum=47,meltTempNum2=53,minMeltDiff=1.25,minAmp=10000,baselineTime=4),
    'STATH'=list(fl='587nm',minFoldIncrease=1.5,meltTempNum=1,meltTempNum2=60,minMeltDiff=1.5,baselineTime=4),
    'As1e'=list(fl='623nm',minFoldIncrease=2,meltTempNum=38,meltTempNum2=50,minMeltDiff=1.2,minAmp=10000,baselineTime=4),
    'Penn'=list(fl='682nm',minFoldIncrease=2,minAmp=20000,meltTempNum=1,meltTempNum2=65,minMeltDiff=2,baselineTime=4)
  )
  amped<-do.call(cbind,lapply(structure(names(args),.Names=names(args)),function(xx){
    do.call(checkAmps,c(lamp[c('lamp','melt')],args[[xx]]))
  }))
  if('target' %in% colnames(lamp$lamp)){
    lookup<-unique(lamp$lamp[,c('target','Well')])
    rownames(lookup)<-lookup$Well
    amped$target<-lookup[as.character(rownames(amped)),'target']
    #amped$target<-sapply(rownames(amped),function(xx)lamp$lamp$target[lamp$lamp$Well==xx][1])
  }else{
    amped$target<-rownames(amped)
  }
  if('enzyme' %in% colnames(lamp$lamp)){
    lookup<-unique(lamp$lamp[,c('enzyme','Well')])
    rownames(lookup)<-lookup$Well
    amped$enzyme<-lookup[as.character(rownames(amped)),'enzyme']
    #amped$enzyme<-sapply(rownames(amped),function(xx)lamp$lamp$enzyme[lamp$lamp$Well==xx][1])
  }
  pos<-do.call(cbind,lapply(structure(names(args),.Names=names(args)),function(xx)tapply(amped[,sprintf('%s.isGood',xx)],amped[,'target'],sum)))
  return(list('amped'=amped,'pos'=pos))
}
