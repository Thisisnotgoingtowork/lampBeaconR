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
    if(grepl('.xlsx?$',meltFile))tmp<-as.data.frame(readxl::read_excel(meltFile,meltRawTab,skip=skip))
    else tmp<-read.csv(meltFile,stringsAsFactors=FALSE,skip=skip)
    colnames(tmp)<-sub('[ -]','.',colnames(tmp))
    ts<-unique(tmp[tmp$Well.Position=='A1','Temperature'])
  }
  mCols<-colnames(melt)[grep('x[0-9]+\\.m',colnames(melt))]
  for(ii in mCols)melt[,ii]<-as.numeric(gsub(',','',melt[,ii]))
  melt$col<-as.numeric(sub('[A-Z]','',melt$Well.Position))
  melt$row<-trimws(sub('[0-9]+','',melt$Well.Position))
  melt$rowNum<-sapply(melt$row,function(xx)which(LETTERS==xx))
  melt$well<-trimws(melt$Well.Position)
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
plotSummary2<-function(melt,xCol,xlab,fls,cols,showLegend=TRUE,plotCol='row',lineCol='col',legendInset=c(-.16,-.24),ncol=2,sameY=FALSE,showMain=TRUE,yScale=1000,ylab='',legendArgs=list(),indicate=NULL,mainExtra='',logXY='',ylims=NULL,lwd=2,plotPoints=NULL,addYFirst=FALSE,cex.lab=1,...){
  if(length(fls)==1)fls<-structure(rep(fls,length(unique(melt[,plotCol]))),.Names=unique(melt[,plotCol]))
  if(length(mainExtra)==1)mainExtra<-structure(rep(mainExtra,length(unique(melt[,plotCol]))),.Names=unique(melt[,plotCol]))
  plots<-unique(melt[,plotCol])
  for(ii in plots){
    fl<-fls[ii]
    thisDat<-melt[melt[,plotCol]==ii,]
    if(is.null(ylims)){
      if(sameY)ylim<-range(melt[,fl])
      else ylim<-range(thisDat[,fl])
    }else{
      ylim<-ylims
    }
    main<-sprintf('%s%s%s%s%s',trimws(ii),ifelse(addYFirst,'',' '),ifelse(addYFirst,'',fl),ifelse(mainExtra=='','',' '),mainExtra[ii])
    if(addYFirst&ii==plots[1])lab<-sprintf("%s RLU (x1000)",fl)
    else lab<-ylab
    plot(1,1,type='n',ylim=ylim/yScale,log=logXY,xlim=range(na.omit(thisDat[,xCol])),xlab='',ylab='',las=1,mgp=c(3,.8,0),...)
    title(ifelse(showMain,main,''),line=0.1,cex.main=cex.lab)
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
    fl2<-dnar::withAs(xx=melt[order(melt$Well,melt$temp),],tapply(xx[,fl],xx$Well,function(zz)max(minAmp,zz[meltTempNum2]*minMeltDiff)))
  }
  curveMelt<-fl1>fl2
  lampAmp<-dnar::withAs(xx=lamp[order(lamp$Well,lamp$time),],tapply(xx[,fl],list(xx$Well),function(zz)tail(zz,1)>max(minAmp,zz[baselineTime]*minFoldIncrease)))
  out<-data.frame('amp'=lampAmp,'melt'=curveMelt[names(lampAmp)],'last'=last[names(lampAmp)],'first'=first[names(lampAmp)],row.names=names(lampAmp))
  out$isGood<-out$melt&out$amp
  out
}


readXls<-function(xls,isQS6=FALSE){
  if(dir.exists(xls)){
    file1<-list.files(xls,'_Raw Data_',full.names=TRUE)
    file2<-list.files(xls,'_Melt Curve Raw_',full.names=TRUE)
    file3<-list.files(xls,'_Melt Curve Result',full.names=TRUE)
    lamp<-readLamp(file1,file2,201,skipCycles=0,skip=23,correctCycles=TRUE)
    info<-unique(read.csv(file3,stringsAsFactors=FALSE,skip=23)[,c('Well.Position','Sample')])
    info[,'Sample Name']<-trimws(info$Sample)
    rownames(info)<-info[,'Well.Position']
  }else if(isQS6){
    lamp<-readLamp(xls,xls,201,skipCycles=0,skip=23,correctCycles=TRUE,meltRawTab='Melt Curve Raw')
    info<-unique(as.data.frame(readxl::read_excel(xls,'Melt Curve Result',skip=23))[,c('Well Position','Sample')])
    info[,'Sample Name']<-trimws(info$Sample)
    rownames(info)<-info[,'Well Position']
  }else{
    lamp<-readLamp(xls,xls,201,skipCycles=0,skip=47)
    info<-unique(as.data.frame(readxl::read_excel(xls,'Sample Setup',skip=47))[,c('Well Position','Sample Name')])
    rownames(info)<-info[,'Well Position']
  }
  lamp<-lapply(lamp[1:2],function(xx){xx$target<-info[xx$well,'Sample Name'];xx$dummy<-1;return(xx[order(xx$target),])})
  lamp<-lapply(lamp,function(xx){xx[!is.na(xx$target),]})
  lamp
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
    'nm587'=checkAmps(lamp$lamp,lamp$melt,'587nm',minFoldIncrease=1.5,meltTempNum=25,meltTempNum2=85,minMeltDiff=1.5,baselineTime=4,isTemp=TRUE),
    'nm682'=checkAmps(lamp$lamp,lamp$melt,'682nm',minFoldIncrease=4,meltTempNum=63,meltTempNum2=75,minMeltDiff=1.2,minAmp=10000,baselineTime=4,isTemp=TRUE),
    'nm520'=checkAmps(lamp$lamp,lamp$melt,'520nm',minFoldIncrease=2,minAmp=20000,meltTempNum=25,meltTempNum2=90,minMeltDiff=1.5,baselineTime=4,isTemp=TRUE)
  )
  amped$target<-sapply(rownames(amped),function(xx)lamp$lamp$target[lamp$lamp$Well==xx][1])
  pos<-data.frame(
    'STATH'=tapply(amped$nm587.isGood,amped[,c('target')],sum),
    'As1e'=tapply(amped$nm682.isGood,amped[,c('target')],sum),
    'Penn'=tapply(amped$nm520.isGood,amped[,c('target')],sum)
  )
  return(list('amped'=amped,'pos'=pos))
}
plotPats<-function(lamp,pos,primers=c('E1'='520nm','STATH'='587nm','As1e'='623nm','Penn'='682nm'),plotMelts=TRUE){
  nPlot<-length(primers)*ifelse(plotMelts,2,1)
  nCol<-length(unique(lamp$lamp$target))
  sameY<-TRUE
  layout(cbind(0,rbind(matrix(1:(nPlot*nCol),nrow=nPlot,byrow=TRUE),0)),height=c(rep(1,nPlot),.1),width=c(.4,rep(1,nCol)))
  par(mar=c(4,2.5,1.1,1),mgp=c(2.5,1,0))
  for(ii in names(primers)){
    indicate<-if(ii %in% colnames(pos))rownames(pos)[pos[,ii]>0] else c()
    plotSummary2(lamp$lamp,'timeMin','Time (m)',primers[ii],ifelse(tolower(ii)=='sybr9','black',ifelse(tolower(ii)=='stath','blue','red')),plotCol='target',lineCol='dummy',showLegend=FALSE,mainExtra=ii,sameY=sameY,indicate=indicate,addYFirst=TRUE)
    if(plotMelts)plotSummary2(lamp$melt,'temp','Temperature (C)',primers[ii],ifelse(tolower(ii)=='sybr9','black',ifelse(tolower(ii)=='stath','blue','red')),plotCol='target',lineCol='dummy',showLegend=FALSE,mainExtra=ii,sameY=sameY,indicate=indicate,addYFirst=TRUE)
  }
}

runAll<-function(file){
  outFile<-sprintf('%s/screening_%s',dirname(file),sub('.xlsx?','',basename(file)))
  isQS6<-dir.exists(file)||grepl('QuantStudio.*6 Pro',as.data.frame(readxl::read_excel(file,'Raw Data',n_max=6))[6,2])
  lamp<-readXls(file,isQS6=isQS6)
  lamp$melt[,c('587nm','520nm','682nm')][is.na(lamp$melt[,c('587nm','520nm','682nm')])]<-1
  amps<-calcAmps2(lamp)
  pdf(sprintf('%s.pdf',outFile),width=1.4*length(unique(lamp$lamp$target)),height=10)
    plotPats(lamp,amps$pos,c('STATH'='587nm','Penn'='520nm','As1e'='682nm'))
  dev.off()
  write.csv(amps$pos[,colnames(amps$pos)!='E1'],sprintf('%s.csv',outFile))
  print(amps$pos[apply(amps$pos[,-1],1,sum)>0,])
}
