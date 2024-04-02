#' read a list
#'
#' reads a list of raw data and plot it into the correct form
#'
#' @param lamp a list that contains raw data
#' @param rowStandardize a bool that indicate if the data is in the correct form or not
#' @return none
#' @export



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
