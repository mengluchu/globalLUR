#' Plot coefficients of Lasso selected variables
#' @param cvfit the result of cvfit
#' @export

Lassoselected = function( cvfit){
  coefall<- coef(cvfit)[-1] # lambda.1se
  coefselected  <- which(coefall != 0)
  allvarnames = dimnames(coef(cvfit))[[1]][-1]
  selectedvarnames = allvarnames[coefselected]
  selectedcoef = coefall[coefselected]

  ENcoef=selectedcoef
  namesENcoef <-selectedvarnames

  ENcoefdf <- data.frame(x=namesENcoef, y= ENcoef, stringsAsFactors = F)

  ag=ggplot(ENcoefdf, aes(x=x,y=y)) + geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) + labs(title='coefficient elastic net 0.5',y='coefficient')
  print(ag)
}
