#' Associate a List of Styles (and some properties) With a Specified Type of 
#' Approval Bar
#' 
#' @author Andrew Halper
#' @param data Plot data.
#' @param ybottom Approval bar rectangle, vertical bottom extent.
#' @param ytop Approval bar rectangle, vertical top extent.
#' @return A list of styles.
getApprovalBarStyle <- function(data, ybottom, ytop) {
  legend.name <- data[[1]]$legend.name
  
  styles <- switch(
    names(data),
    appr_approved_uv = list(
      rect = list(
        xleft = data[[1]]$x0, xright = data[[1]]$x1,
        ybottom = ybottom, ytop = ytop,
        col = "#228B22", border = "#228B22",
        legend.name = legend.name, where = 'first'
      )
    ),
    appr_inreview_uv = list(
      rect = list(
        xleft = data[[1]]$x0, xright = data[[1]]$x1,
        ybottom = ybottom, ytop = ytop,
        col = "#FFD700", border = "#FFD700",
        legend.name = legend.name, where = 'first'
      )
    ),
    appr_working_uv = list(
      rect = list(
        xleft = data[[1]]$x0, xright = data[[1]]$x1,
        ybottom = ybottom, ytop = ytop,
        col = "#DC143C", border = "#DC143C",
        legend.name = legend.name, where = 'first'
      )
    )
  )
  
  return(styles)
}

#' Apply Styles (and some properties) to Approval Bar Rectangles
#' 
#' @param object A gsplot, plot object.
#' @param data A list of gsplot objects to display on the plot.
#' @return gsplot object with approval bar rectangle styles applied.
applyApprovalBarStyles <- function(object, data) {
  ylim <- ylim(object)$side.2
  ylog <- object$global$par$ylog
  
  if (ylim[1] == ylim[2]) {
    # Cope with the rare case of the time series plot being a horizontal line,
    # in which case we have to preemptively compensate for some y-axis interval
    # defaulting code inside R graphics. The 40% factor here comes from the R
    # source code, last seen at 
    # http://docs.rexamine.com/R-devel/Rgraphics_8h.html#a5233f80c52d4fd86d030297ffda1445e
    if (!isEmptyOrBlank(ylog) && ylog) {
      ylim <- c(10^(0.6 * log10(ylim[1])), 10^(1.4 * log10(ylim[2])))
    }
    else {
      ylim <- c(0.6 * ylim[1], 1.4 * ylim[2])
    }
  }
  # calculate approval bar rectangle, vertical extent
  ybottom <- approvalBarYBottom(ylim, ylog, object$side.2$reverse)
  ytop <- approvalBarYTop(ylim, ylog, object$side.2$reverse)
  
  # for any approval intervals present...
  for (i in grep("^appr_.+_uv$", names(data))) {
    # look up style
    approvalBarStyles <- getApprovalBarStyle(data[i], ybottom, ytop)
    for (j in names(approvalBarStyles)) {
      # apply the styles
      object <- do.call(names(approvalBarStyles[j]),
                        append(list(object = object), approvalBarStyles[[j]]))
    }
  }
  return(object)
}

#' Compute Top Position of Approval Bars
#' 
#' @author Andrew Halper
#' @param lim The \emph{y}-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the \emph{y}-axis is log10
#'   scale: \code{TRUE} => log10; \code{FALSE} => linear.
#' @param reverse A Boolean, indicating whether the \emph{y}-axis is inverted:
#'                \code{TRUE} => inverted \emph{y}-axis; \code{FALSE} => not inverted.
#' @return Approval bar, vertical top extent, in world coordinates.
approvalBarYTop <- function(lim, ylog, reverse) {
  return(approvalBarY(lim, ylog, reverse, 0.0245))
}

#' Compute Bottom Position of Approval Bars
#' 
#' @author Andrew Halper
#' @param lim The \emph{y}-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the \emph{y}-axis is log10 scale:
#'             \code{TRUE} => log10; \code{FALSE} => linear.
#' @param reverse A Boolean, indicating whether the \emph{y}-axis is inverted:
#'                \code{TRUE} => inverted \emph{y}-axis; \code{FALSE} => not inverted.
#' @return Approval bar, vertical bottom extent, in world coordinates.
approvalBarYBottom <- function(lim, ylog, reverse) {
  return(approvalBarY(lim, ylog, reverse, 0.04))
}

#' Compute Top or Bottom Vertical Position of Approval Bars
#' 
#' @author Andrew Halper
#' @param lim The \emph{y}-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the \emph{y}-axis is log10 scale:
#'             \code{TRUE} => log10; \code{FALSE} => linear.
#' @param reverse A Boolean, indicating whether the \emph{y}-axis is inverted:
#'                \code{TRUE} => inverted \emph{y}-axis; \code{FALSE} => not inverted.
#' @param ratio A scaling ratio to adjust top or bottom of approval bar rectangle.
#' @return Approval bar, top or bottom \emph{y}-axis point, in world coordinates.
approvalBarY <- function(lim, ylog = NULL, reverse, ratio) {
  e.0 <- lim[1]
  e.1 <- lim[2]
  
  ylog <- ifelse(isEmptyOrBlank(ylog), FALSE, ylog)
  reverse <- ifelse(isEmptyOrBlank(reverse), FALSE, reverse)
  
  # if this is a log10 y-axis
  if (ylog) {
    y <- 10^(log10(e.0) - ratio * (log10(e.1) - log10(e.0)))
  }
  else {
    y <- e.0 - ratio * (e.1 - e.0)
  }
  
  return(y)
}