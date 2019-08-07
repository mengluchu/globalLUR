#' xgBoost regression trees for LUR
#' @param variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. 'ROAD|pop|temp|wind|Rsp|OMI|eleva|coast'
#' @return plot variable importance and an error matrix
#' @export


xgboost_LUR = function(variabledf, max_depth = 4, eta = 0.02, nthread = 2, nrounds = 300, y_varname = c("day_value", "night_value", "value_mean"), training, test, grepstring = "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...) {
    prenres = paste(y_varname, "|", grepstring, sep = "")
    sub_mat = subset_grep(variabledf, prenres)
    
    pre_mat = sub_mat[training, ]
    y_train = sub_mat[training, y_varname]
    
    x_test = sub_mat[test, ]
    y_test = sub_mat[test, y_varname]
    
    
    df1 = data.table(pre_mat, keep.rownames = F)
    formu = as.formula(paste(y_varname, "~.", sep = ""))
    dfmatrix = sparse.model.matrix(formu, data = df1)[, -1]
    
    outputvec = variabledf[training, y_varname]
    bst <- xgboost(data = dfmatrix, label = outputvec, max_depth = max_depth, eta = eta, nthread = nthread, nrounds = nrounds, verbose = 0)
    
    df_test = data.table(x_test, keep.rownames = F)
    
    dfmatrix_test = sparse.model.matrix(formu, data = df_test)[, -1]
    
    
    xgbpre = predict(bst, dfmatrix_test)
    
    importance <- xgb.importance(feature_names = colnames(dfmatrix), model = bst)
    
    print(importance[1:15, ])
    return(error_matrix(y_test, xgbpre))
    
    
}
