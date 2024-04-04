#' read list of data
#'
#' plot the data in the correct form and be able to use in later
#'
#' @param melt description
#' @param xCol description
#' @param xlab description
#' @param fls description
#' @param cols description
#' @param showLegend a bool value
#' @param plotCol a string to indicate the name tag of the rows
#' @param lineCol a string to indicate the name tag of the cols
#' @param legendInset description
#' @param ncol a number that indicate the number of cols
#' @param sameY description
#' @param showMain a bool value
#' @param yScale description
#' @param ylab description
#' @param legendArgs description
#' @param indicate description
#' @param indicateYellow description
#' @param mainExtra description
#' @param logXY description
#' @param ylims description
#' @param lwd description
#' @param plotPoints description
#' @param addYFirst description
#' @param cex.lab description
#' @param yMaxs description
#' @param yScaleFirst description
#' @param extraCmds description
#'
#'


plotSummary<-function(melt,xCol,xlab,fls,cols,showLegend=TRUE,plotCol='row',lineCol='col',legendInset=NULL,ncol=2,sameY=FALSE,showMain=TRUE,yScale=1000,ylab='',legendArgs=list(),indicate=NULL,indicateYellow=NULL,mainExtra='',logXY='',ylims=NULL,lwd=2,plotPoints=NULL,addYFirst=FALSE,cex.lab=1,yMaxs=NULL,yScaleFirst=FALSE,extraCmds=NULL,...){
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
    if(!is.null(extraCmds))sapply(extraCmds,function(xx)xx())
  }
  if(showLegend){
    if(is.null(legendInset)){
      #need fine adjustment option here?
      do.call(legend,c(list(grconvertX(.99,from='ndc'),grconvertY(0.01,from='ndc'),names(cols),lwd=2,col=cols,inset=legendInset,xpd=NA,ncol=ncol,xjust=1,yjust=0),legendArgs))
    }else{
      #for backwards compatibility
      do.call(legend,c(list('topleft',names(cols),lwd=2,col=cols,inset=legendInset,xpd=NA,ncol=ncol),legendArgs))
    }
  }
}
