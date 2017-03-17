#' Get Approval Bar Configuration
#' 
#' @description Given approval bars, return a named list of gsplot elements to
#'   call.
#' @author Andrew Halper
#' @param approvals A list of data objects relevant to plotting approval
#'   bars. Each list item must be named one of appr_approved_uv, appr_inreview_uv, 
#'   or appr_working_uv. Any other names will result in an error.
#' @param ylim The \emph{y}-axis interval, as ordered pair vector.
#' @param ylog A Boolean truth value. \code{TRUE} indicates the \emph{y}-axis is
#'   referenced to log10; linear \emph{y}-axis otherwise.
#' @return a list of configs (data + style) ready to be added to the gsplot object
#'  for each approval bar. Each config is a list named "rect" with appropriate \code{rect}
#'  arguments and any styles (likely color and border) defined.
getApprovalBarConfig <- function(approvals, ylim, ylog) {
  
  styles <- getApprovalBarStyles()
  
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
  ybottom <- approvalBarYBottom(ylim, ylog)
  ytop <- approvalBarYTop(ylim, ylog)
  
  allConfigs <- list()
  for(i in 1:length(approvals)){
    style <- styles[names(approvals[i])]
    config <- list(
    rect = list(xleft = approvals[[i]]$x0,
                         xright = approvals[[i]]$x1,
                         ybottom = ybottom,
                         ytop = ytop,
                         legend.name = approvals[[i]]$legend.name,
                         where = "first",
                         col = style[[1]]$col,
                         border = style[[1]]$border
    ))
    
    allConfigs <- append(allConfigs, config)
  }
  
  return(allConfigs)
}

#' Get Approval Bar Styles
#'
#' @description Get styling information for approval bar elements.
#' @return A list of styling elements.
getApprovalBarStyles <- function() {
  styles <- list(
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
#' @return Approval bar, vertical top extent, in world coordinates.
approvalBarYTop <- function(lim, ylog) {
  return(approvalBarY(lim, ylog, 0.0245))
}

#' Compute bottom position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @return Approval bar, vertical bottom extent, in world coordinates.
approvalBarYBottom <- function(lim, ylog) {
  return(approvalBarY(lim, ylog, 0.04))
}

#' Compute top or bottom vertical position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param ratio A scaling ratio to adjust top or bottom of approval bar rectangle.
#' @return Approval bar, top or bottom y-axis point, in world coordinates.
approvalBarY <- function(lim, ylog = NULL, ratio) {
  e.0 <- lim[1]
  e.1 <- lim[2]
  
  ylog <- ifelse(isEmptyOrBlank(ylog), FALSE, ylog)
  
  # if this is a log10 y-axis
  if (ylog) {
    y <- 10^(log10(e.0) - ratio * (log10(e.1) - log10(e.0)))
  } else {
    y <- e.0 - ratio * (e.1 - e.0)
  }
  
  return(y)
}