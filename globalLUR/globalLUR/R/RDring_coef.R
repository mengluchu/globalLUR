#' Plot coefficients of roadrings of fitting a linear regression model.
#' @param variabledf dataframe including predictors and dependent variable
#' @param y_var the quote of dependent variable name (e.g. quote (value_mean))
#' @param Road_varname the name of variables contains road buffers, e.g. "ROAD"
#' @param pop_var other variables added to linear regression, in a list or vector if multiple. e.g. popvar = c("pop3k", "RSp", "elevation")
#' @param number_roadtypes number of roadtypes, by default 3
#' @param buffers_in buffers of the inner ring
#' @param buffers_out buffers of the outer ring
#' @export

RDring_coef =function(variabledf, y_var, Road_varname = "ROAD", number_roadtypes = 3,buffers_in,buffers_out, pop_var=NA ){

  #  substitute(y_var)


  rdf2 = create_ring(variabledf,normalize=normalize, number_roadtypes = roadtypes,buffers_in = buffers_in,buffers_out = buffers_out)

  if (!is.na(pop_var))
  {

    addiv = paste("|",paste(pop_var, collapse =  "|"), sep = "")
    grepstring = paste(Road_varname,"|", deparse(y_var),addiv, sep = "")
  }
  else
    grepstring = paste(Road_varname,"|", deparse(y_var), sep = "")

  pre_mat = subset_grep(rdf2,grepstring)
  f = formula(paste(y_var, "~.", sep= ""))
  alm = lm(f , data = pre_mat)
  #print( summary(alm))
  allcoef = coef(alm)[which(grepl(Road_varname,names(coef(alm))))] # take off 1,(2 if there is another parameter)

  n_buffer = length(buffers_in)
  shortname = gsub("^([^_]*_[^_]*)_.*$", "\\1",names(allcoef) )
  coefdf = data.frame(bufferring=rep(c(1:n_buffer),length(allcoef)/n_buffer), coeffi =allcoef, fs=  shortname )
  ggplot(data = coefdf, aes(x=bufferring ,y= coeffi )) +
    geom_line()+facet_grid(facets =fs~., scales="free" )
}
