#' Merge new predictor variables (rasters)
#' @param tomerge the dataframe to merge the raster files to
#' @param rasterfile the raster file to merge
#' @param  varname the name of the new variable
#' @return a dataframe with merged variables
#' @examples
#' merged = mergeraster2file(merged, 'C:/Users/Lu000012/Documents/files/GLOBAL_LUR/predictor variables/elevation.map', 'elevation')

#' @export
mergeraster2file = function(tomerge, rasterfile, varname) {
    r = raster(rasterfile)
    
    plot(r)
    location = data.frame(LONGITUDE = tomerge$LONGITUDE, LATITUDE = tomerge$LATITUDE)
    rex = raster::extract(r, location)
    merged = cbind(tomerge, rex)
    names(merged)[ncol(merged)] = varname
    merged
}
