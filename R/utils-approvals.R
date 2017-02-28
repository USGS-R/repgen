#' Get Approval Bar Configuration
#' 
#' @description Given approval bars, return a named list of gsplot elements to
#'   call.
#' @author Andrew Halper
#' @param approvals A list of data objects relevant to plotting approval
#'   bars.
#' @param ylim The \emph{y}-axis interval, as ordered pair vector.
#' @param ylog A Boolean truth value. \code{TRUE} indicates the \emph{y}-axis is
#'   referenced to log10; linear \emph{y}-axis otherwise.
#' @param reverse A Boolean truth value. \code{TRUE} indicates the \emph{y}-axis
#'   is inverted; not inverted otherwise.
#' @return A named list of gsplot calls. The name is the plotting call to make,
#'   and it points to a list of configuration parameters for that call.
getApprovalBarConfig <- function(approvals, ylim, ylog, reverse) {
  
  styles <- getApprovalBarStyles(approvals)
  
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
  ybottom <- approvalBarYBottom(ylim, ylog, reverse)
  ytop <- approvalBarYTop(ylim, ylog, reverse)
  
  legend.name <- approvals[[1]]$legend.name
  
  config <- switch(
    names(approvals),
    appr_approved_uv = list(
      rect = append(
        list(
          xleft = approvals[[1]]$x0,
          xright = approvals[[1]]$x1,
          ybottom = ybottom, ytop = ytop,
          legend.name = legend.name, where = "first"
        ),
        styles
      )
    ),
    appr_inreview_uv = list(
      rect = append(
        list(
          xleft = approvals[[1]]$x0,
          xright = approvals[[1]]$x1,
          ybottom = ybottom, ytop = ytop,
          legend.name = legend.name, where = "first"
        ),
        styles
      )
    ),
    appr_working_uv = list(rect = append(
      list(
        xleft = approvals[[1]]$x0,
        xright = approvals[[1]]$x1,
        ybottom = ybottom, ytop = ytop,
        legend.name = legend.name, where = "first"
      ),
      styles
    ))
  )
  
  return(config)
}

#' Get Approval Bar Styles
#'
#' @description Get styling information for approval bar elements.
#' @author Andrew Halper
#' @param approvals A list of data objects relevant to plotting approval
#'   bars.
#' @return A list of styling elements.
getApprovalBarStyles <- function(approvals) {
  styles <- switch(
    names(approvals),
    appr_approved_uv = list(col = "#228B22", border = "#228B22"),
    appr_inreview_uv = list(col = "#FFD700", border = "#FFD700"),
    appr_working_uv = list(col = "#DC143C", border = "#DC143C")
  )
  
  return(styles)
}

#' Compute top position of approval bars.
#' @param lim The \emph{y}-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the \emph{y}-axis is log_10 scale: 
#'   TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @return Approval bar, vertical top extent, in world coordinates.
approvalBarYTop <- function(lim, ylog, reverse) {
  return(approvalBarY(lim, ylog, reverse, 0.0245))
}

#' Compute bottom position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @return Approval bar, vertical bottom extent, in world coordinates.
approvalBarYBottom <- function(lim, ylog, reverse) {
  return(approvalBarY(lim, ylog, reverse, 0.04))
}

#' Compute top or bottom vertical position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @param ratio A scaling ratio to adjust top or bottom of approval bar rectangle.
#' @return Approval bar, top or bottom y-axis point, in world coordinates.
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