#' using the stored lamp data, 
#' 
#' having the lamp data and produce a graph pdf
#' @param filename the name of the file to output
#' @param seqCol The list of numbers that indicate the column index
#' @param seqRow the list of numbers that indicate the rows index
#' @param colIds1 the list of strings that have the ids of each col
#' @param colIds2 the second list of string that have ids of each col
#' @export
#' 

lampDataGraphpdf<-function(filename = "demo.pdf", seqCol, seqRow, colIds1, colIds2 = ''){
data$lamp <- data$lamp[data$lamp$col %in% seqCol & data$lamp$rowNum %in% selRow ,]
print(nrow(data$lamp))

data$lamp <- data$lamp[data$lamp$Cycle %in% c(1:30, seq(32, 330, 2)), ]
print(nrow(data$lamp))
# Debug prints
print(unique(data$lamp$row))
print(unique(data$lamp$col))

if (length(unique(data$lamp$row)) == 0 || length(unique(data$lamp$col)) == 0) {
  stop("No valid rows or columns found in the data.")
}

colIds<-paste(colIds1,colIds2)
rowIds<-rep(c("Positive","Negative","Test"),c(2,2,4))
cat(colIds)
cat(rowIds)
data<-lapply(data,function(xx){xx$enzyme<-colIds[xx$col];xx$target<-rowIds[xx$rowNum]; xx})
baseCycle<-30
baseLookup<-structure(data$lamp[data$lamp$Cycle==baseCycle,'520nm'],.Names=data$lamp[data$lamp$Cycle==baseCycle,'well'])
data<-lapply(data,function(xx){xx$`520nm - baseline`<-xx$`520nm`-baseLookup[xx$well];xx})
tapply(paste(data$lamp$target,data$lamp$enzyme),data$lamp[,c('row','col')],unique)
cols<-structure(cols8[c(1,7,8)],.Names=unique(data$lamp$target)) 
pdf(sprintf(filename),width=16,height=5)
nPlot<-2
nCol<-length(unique(data$lamp$enzyme))
sameY<-TRUE
layout(rbind(matrix(1:(nPlot*nCol),nrow=nPlot,byrow=TRUE),0),height=c(rep(1,nPlot),.14))
par(mar=c(3.5,4,1.2,0.5),mgp=c(2.5,1,0))                                                                                                                                                                              
plotSummary2(data$lamp,'Cycle','Cycle','520nm',cols,plotCol='enzyme',lineCol='target',mainExtra='',sameY=sameY,addYFirst=TRUE,ncol=3,extraCmds=list(function()abline(v=30.5,lty=2,col='#00000055')),showLegend=FALSE)
plotSummary2(data$lamp,'Cycle','Cycle','520nm - baseline',cols,plotCol='enzyme',lineCol='target',mainExtra='',sameY=sameY,addYFirst=TRUE,ncol=3,extraCmds=list(function()abline(v=30.5,h=1000,lty=2,col='#00000055')))
dev.off() 
}