#' Regression tree for LUR (using ctree), mainly for visualisation purpose

#' @param  variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast"
#' @return plot the tree and return the ctree object
#' @export
ctree_LUR = function (variabledf, y_varname= c("day_value","night_value", "value_mean"), training, test, grepstring ="ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...)
{

  prenres = paste(y_varname,"|", grepstring, sep = "")
  pre_mat = subset_grep(variabledf[training,], prenres)


  y_test = variabledf[test, y_varname]
  x_test = variabledf[test,  ]

  formu = as.formula(paste(y_varname,"~.", sep = ""))

  cf = ctree(formu,  data=pre_mat)


  pre_rf <-  predict(cf, newdata =x_test  )
  #rf_residual <- pre_rf -  rdf_test$NO2

  print(error_matrix(y_test, pre_rf))

  print( plot(cf,fitmean = T)) #ctree party
 # return(cf)
#  cf2=rpart(formu, data=pre_mat)

}
