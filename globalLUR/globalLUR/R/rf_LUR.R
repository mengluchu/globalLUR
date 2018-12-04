#' Random forest for LUR (using ranger)

#' @param  variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param importance the method used for variable importance, from ranger. e.g. "impurity" calculate gini, which is the default
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast"
#' @return  error matrix, plot selected (min MSE ) coefficients
#' @export


rf_LUR = function (variabledf, y_varname= c("day_value","night_value", "value_mean"), training, test, importance = "impurity", grepstring ="ROAD|pop|temp|wind|Rsp|OMI|eleva|coast")
{
pre_mat = variabledf[training,which(grepl(grepstring , names(variabledf)))]
rf3 <- ranger(variabledf[training,y_varname]~ ., data = pre_mat,importance = importance)
print(rf3)
df = data.frame(importance  = rf3$variable.importance)

imp_plot = ggplot( df, aes(x=reorder(rownames(df) ,importance), y=importance,fill=importance))+
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
plot(imp_plot)
pre_rf <- predictions(predict(rf3, data =variabledf[test,y_varname]))
#rf_residual <- pre_rf -  rdf_test$NO2
error_matrix(y_test_night, pre_rf)
}
