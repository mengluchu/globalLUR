#' Random forest for LUR (using ranger)

#' @param  variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast"
#' @return  error matrix, plot selected (min MSE ) coefficients
#' @export


rf_LUR = function (variabledf, y_varname= c("day_value","night_value", "value_mean"), training, test,  grepstring ="ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...)
{
 prenres = paste(y_varname,"|", grepstring, sep = "")
pre_mat = subset_grep(variabledf[training,], prenres)


y_test = variabledf[test, y_varname]
x_test = variabledf[test,  ]

formu = as.formula(paste(y_varname,"~.", sep = ""))

rf3 <- ranger(formu , data = pre_mat, importance = "impurity")

df = data.frame(imp_val  = rf3$variable.importance)

imp_plot = ggplot(df, aes(x=reorder(rownames(df), imp_val), y=imp_val,fill=imp_val))+
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle(paste("Information Value Summary", y_varname, sep =": "))+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

print(imp_plot)



pre_rf <- predictions(predict(rf3, data = x_test))
#rf_residual <- pre_rf -  rdf_test$NO2
print(error_matrix(y_test, pre_rf))

cf=ctree(formu, data=pre_mat)
#plot(cf, type="simple",           # no terminal plots
#     inner_panel=node_inner(cf,
#                            abbreviate = F,            # short variable names
#                            pval = FALSE,                 # no p-values
#                            id = FALSE),                  # no id of node
#     terminal_panel=node_terminal(cf,
#                                  abbreviate = F,
#                                  digits = 1,                   # few digits on numbers
#                                  fill = c("white"),            # make box white not grey
#                                  id = FALSE)
#)



cf2=rpart(formu, data=pre_mat)
return(list(cf,cf2))
}
