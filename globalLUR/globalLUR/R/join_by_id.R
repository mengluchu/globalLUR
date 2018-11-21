#' @param IDfile file with coordinate and ID that Oliver uses
#' @param coordinatefile file with coordinate and variables values (air quality measurements) but no ID
#' @param file2merge file to merge to, i.e. file with all the variables
#' @param varname the name of the variable in the coordinatefile to merge
#' @return a dataframe
#' @export
#' @example  join_by_id(IDfile= testoaq, coordinatefile=meanoaq, file2merge = merged, varname="day_mean")
#require(sf)
#require(maptools)
#require(sp)
#meanoaq = read.csv("q_day.csv") #locations and values
#testoaq = read.csv("openaqmorethan1000_ch_locations.csv") # ID with locaitons

join_by_id= function(IDfile= testoaq, coordinatefile=meanoaq, file2merge = merged, varname="value")
{
  testoverra = over(IDfile,coordinatefile)
  testsp= spCbind(IDfile, testoverra)
  testsf = as(testsp,"sf")
  Stestsf = testsf[,c("X",variable)]

  mergedfile = merge(file2merge, Stestsf, by.x = "ID", by.y = "X", all.x = TRUE)
  return(mergedfile)
}
