#' Plot R sqaure with each predictor.
#' @param predictor_matrix a dataframe of predictors
#' @param  response the response (e.g. air pollution measurement)
#' @return a plot plot individual R square if fitting a linear regression
#' @export
plot_rsq = function(predictor_matrix, response)
{
  returnrsq = function(x,y)
  {lmfit  = lm(y~  x)
  summary(lmfit)$adj.r.square}

  rsq =  apply(predictor_matrix, 2, returnrsq, response)
  a = data.frame(rsq)
  ag = ggplot(a,aes(x=row.names(a),y=rsq))+geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  print(ag)
}
