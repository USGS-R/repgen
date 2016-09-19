#' Associate a list of styles (and some properties) with specified type of
#' approval bar.
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
        col = "lightskyblue", border = "lightskyblue",
        legend.name = legend.name, where = 'first'
      )
    ),
    appr_inreview_uv = list(
      rect = list(
        xleft = data[[1]]$x0, xright = data[[1]]$x1,
        ybottom = ybottom, ytop = ytop,
        col = "yellow2", border = "yellow2",
        legend.name = legend.name, where = 'first'
      )
    ),
    appr_working_uv = list(
      rect = list(
        xleft = data[[1]]$x0, xright = data[[1]]$x1,
        ybottom = ybottom, ytop = ytop,
        col = "lightpink", border = "lightpink",
        legend.name = legend.name, where = 'first'
      )
    )
  )
  
  return(styles)
}
