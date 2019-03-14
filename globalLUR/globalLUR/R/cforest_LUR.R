#' Random forest for LUR (using cforest)

#' @param  variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast"
#' @return  plot variable importance and an error matrix
#' @export


cforest_LUR = function (variabledf, y_varname= c("day_value","night_value", "value_mean"), training, test,  grepstring ="ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...)
{
  prenres = paste(y_varname,"|", grepstring, sep = "")
  pre_mat = subset_grep(variabledf[training,], prenres)


  y_test = variabledf[test, y_varname]
  x_test = variabledf[test,  ]

  formu = as.formula(paste(y_varname,"~.", sep = ""))

  rf3 <- cforest(formu , data = pre_mat )

  df = data.frame(imp_val  = varimp(rf3))

  imp_plot = ggplot(df, aes(x=reorder(rownames(df), imp_val), y=imp_val,fill=imp_val))+
    geom_bar(stat="identity", position="dodge")+ coord_flip()+
    ylab("Variable Importance")+
    xlab("")+
    ggtitle(paste("Information Value Summary", y_varname, sep =": "))+
    guides(fill=F)+
    scale_fill_gradient(low="red", high="blue")

  print(imp_plot)

  pre_rf <- predictions(predict(rf3, data =x_test  ))
  #rf_residual <- pre_rf -  rdf_test$NO2

  return(error_matrix(y_test, pre_rf))


}
