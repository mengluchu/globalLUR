#' read predictor values at the point locations, returns a dataframe or save into a csv
#'@param predictorstack raster stack to read point values from
#'@param pointdatafile the file contains point locations to be read
#'@param coordslonlat a vector specify the names of the longitude and latitude variable names.
#'@param proj the projection of the point file
#'@param csvname the name to save the csv file
#'@export
#'@example
#' lus = raster("/data/lu01/NWA/predictor/NLstack.grd")
#' lf_lo = list.files("/data/lu01/NWA/Bakfietsdata", pattern = "^.*morning.*.csv$", full.names = T)
#' bakfile1 = read.csv(lf_lo[1])
#' proj = "+proj=longlat +datum=WGS84"
#' retrieve_predictor(lus, bakfile1, c("Lon", "Lat"), proj)

retrieve_predictor = function(predictorstack, pointdatafile, coordslonlat, proj, csvname= substr(pointdatafile, 30,34))
{
  sploc  = pointdatafile %>%
  st_as_sf(coords = coordslonlat) %>%
  `st_crs<-` (proj) %>%
  st_transform(crs(lus)@projargs) %>%
  sf:::as_Spatial

  p1 = extract(predictorstack, sploc, sp=T)
  write.csv(p1, paste0(csvname, ".csv"))
}

#openaq
#oaq6 = read.csv("/data/lu01/NWA/openaqNL3mo.csv")
#coordinates(oaq6) = ~longitude+latitude
#proj4string(oaq6) ="+proj=longlat +datum=WGS84"
#oaq6 = spTransform(oaq6, crs(lus)@projargs)

#p2 = extract(lus, oaq6, sp=T)
#write.csv(p2, paste0(substr(lf_lo[1], 30,34), "oaq.csv"))



