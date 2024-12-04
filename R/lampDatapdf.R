#' 
#' read the 
#' 
#' 
#' 
#' 
lampDatapdf <- function(fileName,data, seqCol = seq(1, 12, 1), selRow = seq (1,8,1 )) {
  print(nrow(data$lamp))
  print(selRow)
  print(summary(data$lamp$rowNum %in% selRow))
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
  
  wells <- unique(data$lamp$well[order(data$lamp$row, data$lamp$col)])
  ylim <- range(data$lamp[, '520nm'] / 1000, na.rm = TRUE)
  
  pdf(fileName, height = 10, width = 12)
  par(mfrow = c(length(unique(data$lamp$row)), length(unique(data$lamp$col))), 
      mar = c(4, 3.7, 1, 0), las = 1, mgp = c(2.5, .4, 0))
  
  for (well in wells) {
    index <- data$lamp$well == well
    thisWell <- data$lamp[index, '520nm'] / 1000
    plot(thisWell, main = well, type = 'l', col = 'red',
         ylim = ylim, ylab = 'Fluorescence', xlab = '')
    title(xlab = 'Cycle', mgp = c(1.5, .4, 0))
  }
  
  dev.off()
}

