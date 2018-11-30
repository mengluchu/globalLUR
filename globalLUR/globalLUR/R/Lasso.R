#' Lasso model for LUR, crossvalidation, default fold (10)

#' @param  variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @return  error matrix, plot selected (min MSE ) coefficients
#' @export


Lasso = function (variabledf, y_varname= c("day_value","night_value", "value_mean"), training, test)
{
  pre_mat = variabledf[,which(grepl("ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", names(variabledf)))]
  pre_mat_tr = pre_mat[training,]
  pre_mat_test = pre_mat[test,]
  y_tr_value = variabledf[training, y_varname]
  y_test_value = variabledf[test,y_varname]

  cvfit <- glmnet::cv.glmnet(as.matrix(pre_mat_tr),y_tr_value ,type.measure = "mse",standardize=TRUE,alpha = 0.5,lower.limit=0)

  Lassoselected(cvfit)

  elastic_pred = predict(cvfit, newx=as.matrix(pre_mat_test))
  error_matrix(y_test_value,elastic_pred)
}
