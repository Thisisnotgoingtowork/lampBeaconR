#https://sashamaps.net/docs/tools/20-colors/
cols8<-c('#e6194b', '#f58231', '#ffe119', '#bfef45', '#3cb44b','#42d4f4','#4363d8','#000000')





findWellLine<-function(xls) suppressMessages(which(as.data.frame(readxl::read_excel(xls,'Raw Data',n_max=50))[,1]=='Well'))
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
calcAmpsSNP<-function(lamp){
  amped<-cbind(
    'nm587'=checkAmps(lamp$lamp,lamp$melt,'587nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=85,minMeltDiff=2,minAmp=20000,baselineTime=4,isTemp=TRUE),
    'nm682'=checkAmps(lamp$lamp,lamp$melt,'682nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=85,minMeltDiff=2,minAmp=100000,baselineTime=4,isTemp=TRUE),
    'nm520'=checkAmps(lamp$lamp,lamp$melt,'520nm',minFoldIncrease=3,minAmp=100000,meltTempNum=25,meltTempNum2=80,minMeltDiff=1.2,baselineTime=4,isTemp=TRUE)
  )
  amped$target<-sapply(rownames(amped),function(xx)lamp$lamp$target[lamp$lamp$Well==xx][1])
  if('enzyme' %in% colnames(lamp$lamp))amped$enzyme<-sapply(rownames(amped),function(xx)lamp$lamp$enzyme[lamp$lamp$Well==xx][1])
  amped$row<-sapply(rownames(amped),function(xx)lamp$lamp$row[lamp$lamp$Well==xx][1])
  amped$col<-sapply(rownames(amped),function(xx)lamp$lamp$col[lamp$lamp$Well==xx][1])
  pos<-data.frame(
    'STATH'=tapply(amped$nm587.isGood,amped[,c('target')],sum),
    'As1e'=tapply(amped$nm682.isGood,amped[,c('target')],sum),
    'Penn'=tapply(amped$nm520.isGood,amped[,c('target')],sum)
  )
  return(list('amped'=amped,'pos'=pos))
}
plotPats<-function(lamp,pos=NULL,primers=c('E1'='520nm','STATH'='587nm','As1e'='623nm','Penn'='682nm'),plotMelts=!is.null(lamp$melt),xVar='timeMin',xlab='Time (m)',minorPos=NULL,nPages=1){
  nPlot<-length(primers)*ifelse(plotMelts,2,1)
  sameY<-TRUE
  nCol<-ceiling(length(unique(lamp$lamp$target))/nPages)
  layout(cbind(0,rbind(matrix(1:(nPlot*nCol),nrow=nPlot,byrow=TRUE),0)),height=c(rep(1,nPlot),.1),width=c(.4,rep(1,nCol)))
  par(mar=c(4,0,1.1,0),mgp=c(2.5,1,0))
  allId<-unique(lamp$lamp[,'target'])
  pageId<-split(allId, ceiling(seq_along(allId)/ceiling(length(allId)/nPages)))
  maxs<-sapply(structure(primers,.Names=primers),function(xx)max(lamp$lamp[,xx]))
  if(plotMelts)meltMaxs<-sapply(structure(primers,.Names=primers),function(xx)max(lamp$melt[,xx]))
  for(page in 1:nPages){
    thisDat<-lapply(lamp,function(xx)xx[xx[,'target'] %in% pageId[[page]],])
    for(ii in names(primers)){
      indicate<-if(ii %in% colnames(pos))rownames(pos)[pos[,ii]>0] else c()
      indicateYellow<-if(ii %in% colnames(minorPos))rownames(minorPos)[minorPos[,ii]>0] else c()
      plotSummary2(thisDat$lamp,xVar,xlab,primers[ii],ifelse(tolower(ii)=='sybr9','black',ifelse(tolower(ii)=='stath','blue','red')),plotCol='target',lineCol='dummy',showLegend=FALSE,mainExtra=sprintf('\n%s',ii),sameY=sameY,indicate=indicate,indicateYellow=indicateYellow,addYFirst=TRUE,yScaleFirst=TRUE,yMaxs=pmax(maxs[primers],c('587nm'=2e4,'520nm'=2e5,'682nm'=2e5)[primers],na.rm=TRUE))
      if(length(pageId[[page]])<nCol)for(jj in 1:(nCol-length(pageId[[page]])))plot(1,1,type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
      if(plotMelts){
        plotSummary2(thisDat$melt,'temp','Temperature (C)',primers[ii],ifelse(tolower(ii)=='sybr9','black',ifelse(tolower(ii)=='stath','blue','red')),plotCol='target',lineCol='dummy',showLegend=FALSE,mainExtra=sprintf('\n%s',ii),sameY=sameY,indicate=indicate,indicateYellow=indicateYellow,addYFirst=TRUE,yScaleFirst=TRUE,yMaxs=pmax(maxs[primers],c('587nm'=2e4,'520nm'=2e5,'682nm'=2e5)[primers],na.rm=TRUE))
        if(length(pageId[[page]])<nCol)for(jj in 1:(nCol-length(pageId[[page]])))plot(1,1,type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
      }
    }
  }
}

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


readStepOne<-function(rawFile,rawDataTab='Raw Data',skip=7,cycles=120,minPerCycle=.5){
  dat<-as.data.frame(readxl::read_excel(rawFile,rawDataTab,skip=skip))
  dat$col<-as.numeric(sub('[A-Z]','',dat$Well))
  dat$row<-trimws(sub('[0-9]+','',dat$Well))
  dat$rowNum<-sapply(dat$row,function(xx)which(LETTERS==xx))
  dat$well<-dat$Well
  lamp<-dat[dat$Cycle<=cycles,]
  lamp$timeMin<-lamp$Cycle*minPerCycle
  for(ii in c('BLUE','GREEN','YELLOW','RED'))lamp[,sprintf('%s - baseline',ii)]<-ave(lamp[,ii],lamp$well,FUN=function(xx)xx-mean(xx[1:5]))
  melt<-dat[dat$Cycle>cycles,]
  return(list('lamp'=lamp,'melt'=melt))
}

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

