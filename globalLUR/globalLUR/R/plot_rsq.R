#' Plot R sqaure with each predictor.
#' @param predictor_matrix a dataframe of predictors
#' @param  response the response (e.g. air pollution measurement)
#' @param rsqdf alternatively can also provide a dataframe with rsq and names of the predictors.
#' @param varname name of the column that contains predictor names
#' @return the dataframe of  r square with each predictor
#' @export

plot_rsq= function(predictor_matrix=NA, response=NA,rsqdf= NA, varname=NA, xlab = NA, ylab = NA)
{
  if (!is.na(rsqdf))
  {
    a5= melt(rsqdf, id = varname)

    ag= ggplot(a5 ,aes(x=vars, y = value , colour= variable)) +
      geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+ labs(x = xlab, y=ylab)
     print(ag)

  }
  else{
  a = univar_rsq(predictor_matrix = predictor_matrix,response = response)
  ag = ggplot(a,aes(x=row.names(a),y=rsq))+geom_point() +labs(x = xlab, y =ylab)+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  print(ag)
  }
}


