#' subset by grep
#' @param df the dataframe to subset
#' @param grepstring the namestring of the colomns to grab, greplstyle
#' @export
subset_grep =  function(df, grepstring)
{
  df[,which(grepl(grepstring, names(df )))]
}
