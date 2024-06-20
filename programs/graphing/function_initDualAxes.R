#Initiate dual axes (when y axes will have different scales!)

initDualAxes <- function(leftaxis = NULL, rightaxis = NULL) {
  if (is.null(leftaxis) | is.null(rightaxis)) {
    stop("Function requires leftaxis range and rightaxis range... An example would be initDualAxes(leftaxis = c(y1,y2), rightaxis = c(y1,y2))")
  }
  limsList <- list("r1" = rightaxis[1], "r2" = rightaxis[2], "l1" = leftaxis[1], "l2" = leftaxis[2])
  return(limsList)
}