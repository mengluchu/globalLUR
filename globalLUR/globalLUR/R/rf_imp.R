#' return variable importance, RF
#' @export
rf_imp = function (variabledf, numtrees = 2000,
                   mtry = 33,  y_varname= c("day_value","night_value", "value_mean"), training, test,  grepstring ="ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...)
{
  prenres = paste(y_varname,"|", grepstring, sep = "")
  pre_mat = subset_grep(variabledf[training,], prenres)

  formu = as.formula(paste(y_varname,"~.", sep = ""))

  rf3 <- ranger(formu , data = pre_mat,  num.trees = numtrees, mtry = mtry, importance = "impurity")

  df = data.frame(imp_val  = rf3$variable.importance)
}
