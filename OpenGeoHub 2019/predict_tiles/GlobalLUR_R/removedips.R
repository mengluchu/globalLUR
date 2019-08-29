#' @title remove dips
#' @param x
#' @detail remove time series points that is less than a certain percentage (by default 15%) of the NO2 measured in the previous of the next time stamp
#' @return time series with dips removed
#' @export
removedips = function(x, perce = 0.15) {
  # x = na.approx(x, rule = 2)

  y = as.numeric(x)
  leng = length(x) - 2
  for (i in 1:leng) {
    ## moving window - check distance
    b = i + 2
    c = i + 1 #center, point to check
    if (any(is.na(x[b]), is.na(x[c]), is.na(x[i])))
      next
    mida = x[c] - x[i]
    midc = x[c] - x[b]
    # Find 20 percent
    threshold1 = -perce * x[i]
    threshold2 = -perce * x[b]
    # check threshold
    if (mida < 0 & midc < 0 & mida < threshold1 | midc < threshold2) {
      y[c] = NA
    }
  }
  return(y)
}
