#' Join variables by ID, requires an ID file with coordinates
#' @param IDfile sp file with coordinate and ID that Oliver uses
#' @param coordinatefile sp file with coordinate and variables values (air quality measurements) but no ID
#' @param file2merge dataframe to merge to, i.e. file with all the variables
#' @param varname the name of the variable in the coordinatefile to merge
#' @return a dataframe
#' @export
#' @example  join_by_id(IDfile= testoaq, coordinatefile=meanoaq, file2merge = merged, varname="day_mean")
#require(sf)
#require(maptools)
#require(sp)
#meanoaq = read.csv("q_day.csv") #locations and values
#testoaq = read.csv("openaqmorethan1000_ch_locations.csv") # ID with locaitons

join_by_id= function(IDfile , coordinatefile, file2merge = merged, varname="value", newname)
{
  if(is.data.frame(IDfile)){
    coordinates(IDfile) = ~longitude + latitude
    warning("coordinate variable names should be exactly longitude and latitude")
  }
  if(is.data.frame(coordinatefile)){
    coordinates(coordinatefile) = ~longitude + latitude
    warning("coordinate variable names should be exactly longitude and latitude")
  }

  testoverra = over(IDfile,coordinatefile)
  testsp= data.frame(spCbind(IDfile, testoverra))

  Stestsf = testsp[,c("X", varname)]
  names(Stestsf)=c("X", newname)

  mergedfile = merge(file2merge, Stestsf, by.x = "ID", by.y = "X", all.x = TRUE)
  return(mergedfile)
}
