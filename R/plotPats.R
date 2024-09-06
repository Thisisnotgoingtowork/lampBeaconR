

#https://sashamaps.net/docs/tools/20-colors
#Color list for references
cols8<-c('#e6194b', '#f58231', '#ffe119', '#bfef45', '#3cb44b','#42d4f4','#4363d8','#000000')

#' Reading lamp data 
#' 
#' Reading a list that have lamp data and plot the information into table format
#' @param lamp a list of lamp data
#' @param pos position initial with NULL
#' @param primers a list that have the primers used in the lamp, initial with 'E1'='520nm','STATH'='587nm','As1e'='623nm','Penn'='682nm'
#' @param plotMelts a bool value that check if the lamp list is empty or not
#' @param xVar A string that label the y tag, initial with timeMin
#' @param xlab a string that label the x tag, initial with Time(m)
#' @param minorPos not sure what that is 
#' @param nPage an int value
#' @export
#' 
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
