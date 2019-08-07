#' return R sqaure with each predictor.
#' @param predictor_matrix a dataframe of predictors
#' @param  response the response (e.g. air pollution measurement)
#' @return the dataframe of r square with each predictor
#' @export

univar_rsq = function(predictor_matrix, response) {
    returnrsq = function(x, y) {
        lmfit = lm(y ~ x)
        summary(lmfit)$adj.r.square
    }
    rsq = apply(predictor_matrix, 2, returnrsq, response)
    a = data.frame(rsq)
    
    return(a)
}
