#https://sashamaps.net/docs/tools/20-colors/
cols8<-c('#e6194b', '#f58231', '#ffe119', '#bfef45', '#3cb44b','#42d4f4','#4363d8','#000000')

readLamp<-function(rawFile,meltFile,meltStart=121,timePerCyle=30,skipCycles=0,skip=21,sep=',',correctCycles=FALSE,rawDataTab='Raw Data',meltRawTab='Melt Curve Raw Data'){
  if(grepl('.xlsx?$',rawFile)){
    melt<-as.data.frame(readxl::read_excel(rawFile,rawDataTab,skip=skip))
    colnames(melt)<-sub('[- ]','.',colnames(melt))
  }else{
    melt<-read.csv(rawFile,skip=skip,sep=sep,stringsAsFactors=FALSE) 
  }
  if(is.numeric(meltFile)|is.null(meltFile)){
    ts<-meltFile
  }else{
    if(grepl('.xlsx?$',meltFile)){
      if(meltRawTab %in% readxl::excel_sheets(meltFile)) tmp<-as.data.frame(readxl::read_excel(meltFile,meltRawTab,skip=skip))
      else tmp<-data.frame('Temperature'=NA)[0,,drop=FALSE]
    }else{
      tmp<-read.csv(meltFile,stringsAsFactors=FALSE,skip=skip)
    }
    colnames(tmp)<-sub('[ -]','.',colnames(tmp))
    ts<-unique(tmp[tmp$Well.Position=='A1','Temperature'])
  }
  mCols<-colnames(melt)[grep('x[0-9]+\\.m',colnames(melt))]
  for(ii in mCols)melt[,ii]<-as.numeric(gsub(',','',melt[,ii]))
  melt$col<-as.numeric(sub('[A-Z]','',melt$Well.Position))
  melt$row<-trimws(sub('[0-9]+','',melt$Well.Position))
  melt$rowNum<-sapply(melt$row,function(xx)which(LETTERS==xx))
  melt$well<-trimws(melt$Well.Position)
  melt$file<-rawFile
  #assumes in correct order
  if(correctCycles)melt$Cycle<-ave(melt$Cycle,melt$Well.Position,FUN=function(xx)1:length(xx))
  melt$Cycle<-melt$Cycle-skipCycles
  melt<-melt[melt$Cycle>0,]
  colnames(melt)<-sub('^[Xx]([0-9])[._][mM]([0-9])','x\\1.m\\2',colnames(melt))
  flNames<-c('x1.m1'='520nm','x2.m2'='558nm','x3.m3'='587nm','x4.m4'='623nm','x5.m5'='682nm','x6.m6'='711nm')
  flNames<-flNames[names(flNames) %in% colnames(melt)]
  for(ii in names(flNames))melt[,flNames[ii]]<-melt[,ii]
  lamp<-melt[melt$Cycle<meltStart,]
  lamp$time<-lamp$Cycle*timePerCyle
  lamp$timeMin<-lamp$Cycle*timePerCyle/60
  meltCurve<-melt[melt$Cycle %in% (meltStart+1:length(ts)-1),]
  meltCurve$temp<-ts[meltCurve$Cycle-meltStart+1]
  if(length(ts)==0)meltCurve<-NULL
  extra<-melt[melt$Cycle > meltStart+length(ts)-1,]
  return(list(lamp=lamp,melt=meltCurve,extra=extra))
}
plotLamp<-function(lamp,rowStandardize=FALSE){
  mCols<-colnames(lamp)[grep('x[0-9]+\\.m',colnames(lamp))]
  mCol<-structure(dnar::rainbow.lab(length(mCols)),.Names=mCols)
  showMelt<-!is.null(lamp)
  par(mfrow=c(8,12))
  for(jj in mCols){
    for(ii in unique(lamp$well)){
      if(rowStandardize)ylim<-range(lamp[lamp$row==lamp[lamp$well==ii,'row'],jj])
      else ylim<-range(lamp[,jj])
      thisDat<-lamp[lamp$well==ii,]
      plot(1,1,type='n',main=sprintf('%s %s',ii,jj),ylim=ylim,xlim=range(thisDat$Cycle),xlab='"Cycle"',ylab='',las=1)
      lines(thisDat[,'Cycle'],thisDat[,jj],col=mCol[jj],lwd=4)
    }
  }
}
plotSummary<-function(melt,xCol,xlab,fls,rowIds,colIds,cols,showLegend=TRUE,plotCol='row',lineCol='col',legendInset=c(-.16,-.24),ncol=2,sameY=FALSE,showMain=TRUE,yScale=1000,ylab='',...){
  tmp<-melt
  tmp[,'PLOTCOL_XXX']<-rowIds[tmp[,plotCol]]
  tmp[,'LINECOL_XXX']<-colIds[tmp[,lineCol]]
  if(is.null(names(fls)))names(fls)<-1:length(fls)
  if(is.null(names(rowIds)))names(rowIds)<-1:length(rowIds)
  fls<-tapply(fls,rowIds[names(fls)],unique)
  if(any(sapply(fls,length)!=1))stop('Ambiguous fls')
  plotSummary2(tmp,xCol,xlab,fls,cols,showLegend,'PLOTCOL_XXX','LINECOL_XXX',legendInset,ncol,sameY,showMain,yScale,ylab,...)
}
plotSummary2<-function(melt,xCol,xlab,fls,cols,showLegend=TRUE,plotCol='row',lineCol='col',legendInset=c(-.16,-.24),ncol=2,sameY=FALSE,showMain=TRUE,yScale=1000,ylab='',legendArgs=list(),indicate=NULL,indicateYellow=NULL,mainExtra='',logXY='',ylims=NULL,lwd=2,plotPoints=NULL,addYFirst=FALSE,cex.lab=1,yMaxs=NULL,yScaleFirst=FALSE,...){
  if(length(fls)==1)fls<-structure(rep(fls,length(unique(melt[,plotCol]))),.Names=unique(melt[,plotCol]))
  if(length(mainExtra)==1)mainExtra<-structure(rep(mainExtra,length(unique(melt[,plotCol]))),.Names=unique(melt[,plotCol]))
  plots<-unique(melt[,plotCol])
  for(ii in plots){
    fl<-fls[ii]
    thisDat<-melt[melt[,plotCol]==ii,]
    if(is.null(ylims)){
      if(sameY)ylim<-c(min(melt[,fl]),max(c(melt[,fl],yMaxs[fl])))
      else ylim<-range(thisDat[,fl])
    }else{
      ylim<-ylims
    }
    main<-sprintf('%s%s%s%s%s',trimws(ii),ifelse(addYFirst,'',' '),ifelse(addYFirst,'',fl),ifelse(mainExtra[ii]=='','',' '),mainExtra[ii])
    if(addYFirst&ii==plots[1])lab<-sprintf("%s RLU (x1000)",fl)
    else lab<-ylab
    plot(1,1,type='n',ylim=ylim/yScale,log=logXY,xlim=range(na.omit(thisDat[,xCol])),xlab='',ylab='',las=1,mgp=c(3,.8,0),yaxt=ifelse(yScaleFirst&ii!=plots[1],'n','s'),...)
    title(ifelse(showMain,main,''),line=0.1-nchar(gsub('[^\n]+','',main)),cex.main=cex.lab)
    title(xlab=xlab,mgp=c(2.4,1,0),cex.lab=cex.lab)
    title(ylab=lab,mgp=c(3.1,1,0),xpd=NA,cex.lab=cex.lab)
    for(kk in unique(thisDat[,lineCol])){
      thisCol<-thisDat[thisDat[,lineCol]==kk,]
      for(ll in unique(thisCol$Well))lines(thisCol[thisCol$Well==ll,xCol],thisCol[thisCol$Well==ll,fl]/yScale,col=cols[kk],lwd=lwd)
    }
    if(!is.null(plotPoints)){
      thisPoints<-plotPoints[plotPoints[,plotCol]==ii,]
      for(kk in unique(thisDat[,lineCol])){
        thisPoint<-thisPoints[thisPoints[,lineCol]==kk,]
        for(ll in unique(thisPoints$Well))points(thisPoint[thisPoint$Well==ll,xCol],thisPoint[thisPoint$Well==ll,fl]/yScale,col=cols[kk],lwd=lwd)
      }
    }
    if(ii %in% indicate){
      box('figure',lwd=6,col=cols[kk])
    }else if(ii %in% indicateYellow){
      box('figure',lwd=4,col='gold')
    }
  }
  if(showLegend)do.call(legend,c(list('topleft',names(cols),lwd=2,col=cols,inset=legendInset,xpd=NA,ncol=ncol),legendArgs))
}


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
  lamp<-lapply(lamp,function(xx){xx[!is.na(xx$target),]})
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
plotPats<-function(lamp,pos=NULL,primers=c('E1'='520nm','STATH'='587nm','As1e'='623nm','Penn'='682nm'),plotMelts=!is.null(lamp$melt),xVar='timeMin',xlab='Time (m)',minorPos=NULL){
  nPlot<-length(primers)*ifelse(plotMelts,2,1)
  nCol<-length(unique(lamp$lamp$target))
  sameY<-TRUE
  layout(cbind(0,rbind(matrix(1:(nPlot*nCol),nrow=nPlot,byrow=TRUE),0)),height=c(rep(1,nPlot),.1),width=c(.4,rep(1,nCol)))
  par(mar=c(4,0,1.1,0),mgp=c(2.5,1,0))
  for(ii in names(primers)){
    indicate<-if(ii %in% colnames(pos))rownames(pos)[pos[,ii]>0] else c()
    indicateYellow<-if(ii %in% colnames(minorPos))rownames(minorPos)[minorPos[,ii]>0] else c()
    plotSummary2(lamp$lamp,xVar,xlab,primers[ii],ifelse(tolower(ii)=='sybr9','black',ifelse(tolower(ii)=='stath','blue','red')),plotCol='target',lineCol='dummy',showLegend=FALSE,mainExtra=sprintf('\n%s',ii),sameY=sameY,indicate=indicate,indicateYellow=indicateYellow,addYFirst=TRUE,yScaleFirst=TRUE,yMaxs=c('587nm'=2e4,'520nm'=2e5,'682nm'=2e5))
    if(plotMelts)plotSummary2(lamp$melt,'temp','Temperature (C)',primers[ii],ifelse(tolower(ii)=='sybr9','black',ifelse(tolower(ii)=='stath','blue','red')),plotCol='target',lineCol='dummy',showLegend=FALSE,mainExtra=sprintf('\n%s',ii),sameY=sameY,indicate=indicate,indicateYellow=indicateYellow,addYFirst=TRUE,yScaleFirst=TRUE)
  }
}

runAll<-function(file,outDir=dirname(file),isSAP3=grepl('SAPv?3',file),nCycle=200){
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
      'STATH'=list('fl'='587nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=78,minMeltDiff=2,minAmp=20000,baselineTime=4,isTemp=TRUE),
      'As1e'=list('fl'='682nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=72,minMeltDiff=2,minAmp=100000,baselineTime=4,isTemp=TRUE),
      'Penn'=list('fl'='520nm',minFoldIncrease=3,minAmp=100000,meltTempNum=25,meltTempNum2=90,minMeltDiff=1.5,baselineTime=4,isTemp=TRUE)
    )
    conditionsMaybe<-list(
      'STATH'=list('fl'='587nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=78,minMeltDiff=2,minAmp=Inf,baselineTime=4,isTemp=TRUE),
      'As1e'=list('fl'='682nm',minFoldIncrease=3,meltTempNum=25,meltTempNum2=72,minMeltDiff=2,minAmp=30000,baselineTime=4,isTemp=TRUE),
      'Penn'=list('fl'='520nm',minFoldIncrease=3,minAmp=30000,meltTempNum=25,meltTempNum2=90,minMeltDiff=1.5,baselineTime=4,isTemp=TRUE)
    )
  }
  primers<-sapply(conditions,'[[','fl')
  amps<-do.call(calcAmpsGeneric,c(list(lamp),conditions))
  amps2<-do.call(calcAmpsGeneric,c(list(lamp),conditionsMaybe))
  amps2$pos[amps$pos>0]<-0
  pdf(sprintf('%s.pdf',outFile),width=1.3*length(unique(lamp$lamp$target)),height=10)
    plotPats(lamp,amps$pos,primers,minorPos=amps2$pos)
  dev.off()
  write.csv(amps$pos[,colnames(amps$pos)!='E1'],sprintf('%s.csv',outFile))
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
findSamples(c('pos_010000','S-210209-03112','S-210209-00889','NOTAREALSAMPLE'),'384_Plate_COVID SAFE Sample 2021_02_10 Stemmler_SampleTemplate.xlsx','badIds.csv')


