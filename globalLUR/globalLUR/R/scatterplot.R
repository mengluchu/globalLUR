#' Plot scatterplot between the dependent variables and all the predictors
#' @param  variabledf the dataframe containing dependent and independent variables
#' @param y_name charater, the name of the dependent variable
#' @param fittingmethod the method to fit a line, choosing from the 'stat_smooth' in gglot2, can also be  lm, glm, gam, loess.
#' @export
#'
scatterplot = function(variabledf, y_name, fittingmethod = "lm") {
    # quote(y_var)
    variabledf %>% gather(VAR, predictors, -y_name) %>% ggplot(aes_string(x = "predictors", y = y_name)) + geom_point() + facet_wrap(~VAR, scales = "free") + stat_smooth(method = fittingmethod) + theme_bw()
}
