#' 
#' reading a csv file** 
#' 
#' reading a csv file** This is an combination of readXls, calcAmps, etc all functions together
#' @param file An excel file that have the raw data
#' @param outDir output dictionary address path in the local system
#' @param isSAP3 Is the file with certain format?**
#' @param nCycle an int that indicate the number of cycle, initial with 200
#' @param censorY a censor**
#' @param nPages an int that says the page number??? initial with 1
#' @export
#' 



runAll<-function(file,outDir=dirname(file),isSAP3=grepl('[Ss][Aa][Pp][Vv]?3',file),nCycle=200,censorY=NULL,nPages=1){
  outFile<-sprintf('%s/screening_%s',outDir,sub('.xlsx?','',basename(file)))
  isQS6<-dir.exists(file)||grepl('QuantStudio.*6 Pro',as.data.frame(readxl::read_excel(file,'Raw Data',n_max=6))[6,2])||!'Melt Curve Raw Data' %in% readxl::excel_sheets(file)
  if(!any(c('Melt Curve Raw','Melt Curve Raw Data') %in% readxl::excel_sheets(file)))extraTemps<-c(95,78,72,62,25)
  else extraTemps<-c()
  lamp<-readXls(file,isQS6=isQS6,extraTemps=extraTemps,nCycle=nCycle)
  lamp$melt[,c('587nm','520nm','682nm')][is.na(lamp$melt[,c('587nm','520nm','682nm')])]<-1
  if(is.null(lamp$melt)||nrow(lamp$melt)==0)stop('Melt curve not detected')
  if(isSAP3){
    conditions<-list(
      'STATH'=list('fl'='587nm',minFoldIncrease=2,meltTempNum=25,meltTempNum2=78,minMeltDiff=2,minAmp=10000,baselineTime=4,isTemp=TRUE),
      'As1e'=list('fl'='520nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=72,minMeltDiff=1.5,minAmp=100000,baselineTime=4,isTemp=TRUE),
      'Penn'=list('fl'='682nm',minFoldIncrease=3,minAmp=100000,meltTempNum=25,meltTempNum2=90,minMeltDiff=2,baselineTime=4,isTemp=TRUE)
    )
    conditionsMaybe<-list(
      'STATH'=list('fl'='587nm',minFoldIncrease=2,meltTempNum=25,meltTempNum2=78,minMeltDiff=2,minAmp=Inf,baselineTime=4,isTemp=TRUE),
      'As1e'=list('fl'='520nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=72,minMeltDiff=1.5,minAmp=30000,baselineTime=4,isTemp=TRUE),
      'Penn'=list('fl'='682nm',minFoldIncrease=3,minAmp=30000,meltTempNum=25,meltTempNum2=90,minMeltDiff=2,baselineTime=4,isTemp=TRUE)
    )
  }else{
    conditions<-list(
      'STATH'=list('fl'='587nm',minFoldIncrease=2,meltTempNum=25,meltTempNum2=78,minMeltDiff=2,minAmp=10000,baselineTime=4,isTemp=TRUE),
      'As1e'=list('fl'='682nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=72,minMeltDiff=2,minAmp=100000,baselineTime=4,isTemp=TRUE),
      'Penn'=list('fl'='520nm',minFoldIncrease=3,minAmp=100000,meltTempNum=25,meltTempNum2=90,minMeltDiff=1.5,baselineTime=4,isTemp=TRUE)
    )
    conditionsMaybe<-list(
      'STATH'=list('fl'='587nm',minFoldIncrease=2,meltTempNum=25,meltTempNum2=78,minMeltDiff=2,minAmp=Inf,baselineTime=4,isTemp=TRUE),
      'As1e'=list('fl'='682nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=72,minMeltDiff=2,minAmp=30000,baselineTime=4,isTemp=TRUE),
      'Penn'=list('fl'='520nm',minFoldIncrease=3,minAmp=30000,meltTempNum=25,meltTempNum2=90,minMeltDiff=1.5,baselineTime=4,isTemp=TRUE)
    )
  }
  primers<-sapply(conditions,'[[','fl')
  amps<-do.call(calcAmpsGeneric,c(list(lamp),conditions))
  amps2<-do.call(calcAmpsGeneric,c(list(lamp),conditionsMaybe))
  amps2$pos[amps$pos>0]<-0
  plotDat<-lamp
  for(ii in names(censorY)){
    isCensor<-any(sapply(lamp,function(xx)any(xx[,ii]>censorY[ii])))
    if(isCensor){
      warning('Censoring ',ii,' values higher than ',censorY[ii],' to ',censorY[ii])
      plotDat<-lapply(plotDat,function(xx){xx[xx[,ii]>censorY[ii],ii]<-censorY[ii];return(xx)})
    }
  }
  pdf(sprintf('%s.pdf',outFile),width=1.3*length(unique(lamp$lamp$target))/nPages,height=10)
  plotPats(plotDat,amps$pos,primers,minorPos=amps2$pos,nPages=nPages)
  dev.off()
  write.csv(amps$pos[,colnames(amps$pos)!='E1'],sprintf('%s.csv',outFile))
  noStath<-rownames(amps$pos)[amps$pos[,'STATH']==0]
  if(length(noStath)>0){
    message('No stath:')
    message(paste(noStath,collapse=', '))
  }
  write.csv(data.frame('id'=noStath),sprintf('%s_noStath.csv',outFile),row.names=FALSE)
  print(amps$pos[apply(amps$pos[,toupper(colnames(amps$pos))!='STATH'],1,sum)>0,])
  print(amps2$pos[apply(amps2$pos[,toupper(colnames(amps$pos))!='STATH'],1,sum)>0,,drop=FALSE])
  invisible(list('lamp'=lamp,'amp'=amps))
}
