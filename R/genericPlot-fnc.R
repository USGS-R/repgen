newGridPlot <- function(lims, log='', ylab="", xlab="", ticks) {
  
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
  abline(h = ticks$yminor, lty = 4, col = "lightgray")
  abline(h = ticks$ymajor, lty = 1, col = "black")
  abline(v = ticks$xminor, lty = 4, col = "lightgray")
  abline(v = ticks$xmajor, lty = 1, col = 'black')
  
  
  
}