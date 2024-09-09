#https://sashamaps.net/docs/tools/20-colors/
cols8<-c('#e6194b', '#f58231', '#ffe119', '#bfef45', '#3cb44b','#42d4f4','#4363d8','#000000')









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

