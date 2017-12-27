getFiveyearStyle <- function() {
  styles <- list(
      stat1_lines = list(type="s", col="blue", pch=20, cex=0.5),
      stat2_lines = list(type="s", col="maroon", pch=20, cex=0.5),
      stat3_lines = list(type="s", col="orange", pch=20, cex=0.5, ann=TRUE, axes=FALSE),
      stat4_lines = list(type="s", col="black", pch=20, cex=0.5, ann=TRUE, axes=FALSE),
      stat1e_lines = list(type="s", col="red", lty=2, pch=20, cex=0.5),
      stat2e_lines = list(type="s", col="red1", lty=3, pch=20, cex=0.5),
      stat3e_lines = list(type="s", col="red2", lty=6, pch=20, cex=0.5),
      stat4e_lines = list(type="s", col="red3", lty=2, pch=20, cex=0.5),
      max_iv_points = list(pch=8, cex=2),
      min_iv_points = list(pch=8, cex=2),
      bottom_iv_label = list(adj = 0, side = 3, axes=FALSE, cex=0.6, line = 0.1),
      top_iv_label = list(adj = 0, side = 3, axes=FALSE, cex=0.6, line = 0.75),
      gw_level_points = list(pch = 8, bg = 'orange', col = 'orange', cex = 1.2, lwd=1, legend.name="Measured Water Level (GWSI)")
  )
  
  return(styles)
}
