#' return variable importance xgboost
#' @export
xgboost_imp = function (variabledf,  max_depth =4, eta =0.02, nthread =2, nrounds = 300, y_varname= c("day_value","night_value", "value_mean"), training, test,  grepstring ="ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...)
{
  prenres = paste(y_varname,"|", grepstring, sep = "")
  sub_mat = subset_grep(variabledf , prenres)
  sub_matx = subset_grep(variabledf , grepstring )
  pre_mat = sub_mat[training,]
  y_train = sub_mat[training,y_varname]

  x_test = sub_mat[test,  ]
  y_test = sub_mat[test, y_varname]


  df1 = data.table(pre_mat, keep.rownames = F)
  formu = as.formula(paste(y_varname,"~.", sep = ""))
  dfmatrix  = sparse.model.matrix(formu , data = df1 )[,-1]

  outputvec = variabledf[training, y_varname]
  bst <- xgboost(data = dfmatrix, label = outputvec, max_depth = max_depth,
                 eta = eta, nthread = nthread, nrounds = nrounds)

  df_test= data.table(x_test, keep.rownames = F)

  dfmatrix_test  = sparse.model.matrix(formu, data = df_test )[,-1]


  importance <- xgb.importance(feature_names = colnames(dfmatrix), model = bst)


  V3=data.frame(importance)
  V3 = V3[,c(1,2)]

  # xgboost return different number of features.
  allfeature = data.frame(Feature = names( sub_matx),
                          Gain = 0 )

  m = merge(allfeature, V3, "Feature", all.x=T)
  rownames(m) = m$Feature
  m = m %>%select(Gain.y)
  m[is.na(m)] = 0
  names(m)= c( "imp_val")
  return(m)
}
