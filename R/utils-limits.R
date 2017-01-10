############ used in uvhydrograph-render and vdiagram-render ############ 

#' @title Test Callouts
#' 
#' @description A function to ensure that callouts on a plot fit within the x-limits.
#' 
#' @param plot_obj The plot object whose callouts will be checked
#' @param xlimits The outer x coordinates on the graph.
#' 
#' @return The plot object changed so that all callouts fit and show on the plot.
#' 
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
testCallouts <- function(plot_obj, xlimits){
  xrange <- diff(xlimits)
  buffer <- 0.04*xrange
  xlow <- xlimits[1]-buffer
  xhigh <- xlimits[2]+buffer
  xlimits_real <- c(xlow, xhigh)
  
  png('TMP_PLOT')
  print(plot_obj)
  width_char <- par("cxy")[1]
  #When you're done
  dev.off()
  #Delete the plot you just generated
  unlink('TMP_PLOT')
  
  i_view12 <- which(names(plot_obj$view.1.2) == "callouts")
  i_view14 <- which(names(plot_obj$view.1.4) == "callouts")
  
  plot_obj <- testCalloutsByView(plot_obj, 'view.1.2', xlimits_real, width_char, xrange)
  plot_obj <- testCalloutsByView(plot_obj, 'view.1.4', xlimits_real, width_char, xrange)
  
  return(plot_obj)
}

#' @title Test Callouts by View
#' 
#' @description A function that calculates if a callout goes past an x-axis limit.
#' 
#' @param plot_obj The overarching plot object that contains these objects.
#' @param view_num The axis which the callouts are being compared to. Starts at the bottom x-axis
#' as 1, and goes clockwise from there. (bottom, left, top, right)
#' @param xlimits_real The x-axis limits (lower and upper limits) at which point callouts are cut off.
#' @param width_char The width of a character on the plot. Used to determine if a string is cut off.
#' @param xrange The limits of the graph not taking into account a small amount of buffer room.
#' 
#' @return The plot object with the callouts changed to not overlap with the x-axis limits.
#' 
testCalloutsByView <- function(plot_obj, view_num, xlimits_real, width_char, xrange){
  
  i_view <- which(names(plot_obj$view_num) == "callouts")
  
  for(i in i_view){
    callout_args <- plot_obj[[view_num]][[i]]
    if (!is.na(xtfrm(callout_args$x[i])) |  
        !is.na(xtfrm(callout_args$y[i])) |
        is.null(xtfrm(callout_args$x[i])) |  
        is.null(xtfrm(callout_args$y[i]))) {  
      text_len <- nchar(callout_args$labels)
      
      len <- ifelse(is.null(callout_args$length), 0.1, callout_args$length)
      
      xend <- len * xrange * cos(2*pi*(30/360))
      xnew <- callout_args$x + xend + (width_char * text_len) 
      tooLong <- xnew > xlimits_real[2]
      
      if(any(tooLong)){
        out <- which(tooLong)
        notout <- which(!tooLong)
        plot_obj[[view_num]][[i]]$angle[notout] <- NA
        plot_obj[[view_num]][[i]]$angle[out] <- 150
      }
    }
  }
  return(plot_obj)
}


#' Rescale top of y-axis to create ~4% margin between vertical top extent of 
#' plot objects and top edge of plot. This is an inaccurate emulation of (the 
#' top-end-of-plot behavior of) R graphics::par's "yaxs = 'r'" state, because we
#' have to use "yaxs = 'i'" in spots, but still want the ~4% margin at the top 
#' of the plot, so we adjust the y-axis endpoint accordingly after we do what we
#' need.
#' @param object A gsplot, plot object.
#' @return The passed-in gsplot object, with y-axis top augmented (upwards).
RescaleYTop <- function(object) {
  ylog <- par("ylog")
  reverse <- object$side.2$reverse
  
  # Desired top margin, in NDCs. See also "yaxs" parameter domain in
  # graphics::par.
  m <- 0.04
  
  # vertical extent and length
  e <- ylim(object)$side.2
  e.length <- abs(e[1] - e[2])
  
  if (ylog) {
    # TODO: the log10 calculations below should probably be upgraded to mirror
    # the form of analogous formulae in the linear-case, conditionals in the
    # block below.
    
    # if the y-axis is inverted
    if (reverse) {
      object$side.2$lim[1] <- 10^((1 - m) * log10(e[2]))
    }
    else {
      object$side.2$lim[2] <- 10^((1 + m) * log10(e[2]))
    }
  }
  else {
    # if the y-axis is inverted
    if (reverse) {
      object$side.2$lim[1] <- e[2] - m * e.length
    }
    else {
      # The 5.14 coefficient below is a hack that was reverse-engineered from 
      # the SVG output (using Inkscape, and the "back-of-the-envelope"). At the 
      # moment, we have no idea why it works, and is likely not robust enough 
      # for production.
      object$side.2$lim[2] <- e[2] + 5.14 * m * e.length
    }
  }
  
  return(object)
}