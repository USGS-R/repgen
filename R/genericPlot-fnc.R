newGridPlot <- function(lims, log='', ylab="", xlab="", ticks, 
                        xlty=c('minor'=1, 'major'=1), xcol=c('minor'="lightgray", 'major'='black'),
                        ylty=c('minor'=4, 'major'=1), ycol=c('minor'="lightgray", 'major'='black')) {
  
  xaxis <- lims$xlim
  yaxis <- lims$ylim
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  
  mn_tck = 50
  mn_tkL = 0.005
  mj_tck = 10
  mj_tkL = 0.01
  ax_lab = 0.55 # scale
  
  
  # main plot area
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, log = log,
       xlab=" ", ylab=ylab, xaxt="n", yaxt="n", mgp=mgp$y, xaxs='i')
  
  # gridlines
  abline(h = ticks$yminor, lty = ylty[['minor']], col = ycol[['minor']])
  abline(h = ticks$ymajor, lty = ylty[['major']], col = ycol[['major']])
  abline(v = ticks$xminor, lty = xlty[['minor']], col = xcol[['minor']])
  abline(v = ticks$xmajor, lty = xlty[['major']], col = xcol[['major']])
  
  # major axes
  axis(side=1, at=ticks$xtickLabel$value, cex.axis=ax_lab, tck=mj_tkL, mgp=mgp$x, labels=ticks$xtickLabel$text)
  axis(side=2, at=ticks$ytickLabel$value, cex.axis=ax_lab, tck=mj_tkL, mgp=mgp$y, labels=ticks$ytickLabel$text)
  
  
}

logTicks <- function(bounds, num_ticks = 5){
  
  bounds <- bounds[is.finite(bounds)]
  bounds <- bounds[bounds>0]
  if (is.null(bounds))
    stop("(logTicks): input data all negative, null or NA")
  
  ticks <- pretty( bounds, num_ticks )

  logRange <- range(log10(bounds), na.rm=TRUE)

  
  v1 <- floor(logRange[1])
  v2 <- ceiling(logRange[2])
  
  ticks <- 10^pretty(c(v1,v2), num_ticks+2)
  allowed <- as.vector(sapply(seq(1,9), FUN =function(x) c(0,10^seq(-10,50)*x)))
  
  ticks <- unique(sapply(ticks, function(x) allowed[which.min(abs(allowed-x))]))
  
  return(ticks)
}

linearTicks <- function(){
  
  
}
