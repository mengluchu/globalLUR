#' convert ppm to ug/m3
#' @param df  values in ppm
#' @return air pollutant in ug/m3
#' @details ug/m3 = ppm * 1000 * 1.91
#'
#' @example
#' library(countrycode)
#' merged$value_mean = ifelse(merged$country%in%countrywithppm,ppm2ug(merged$value_mean), merged$value_mean )
#' @export

ppm2ug = function(ppm) {
    ppm * 1000 * 1.91
}
