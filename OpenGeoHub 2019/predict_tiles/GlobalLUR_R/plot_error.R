#' Plot the prediction errors in space
#' @param validataion The validataion vector
#' @param prediction The prediction vector
#' @param err The error vector, if provided, the "validataion" and "prediction" are not needed.
#' @param geome geometry of an sf
#' @param extremequantil_low the low quantile to filter out for better visualization
#' @param extremequantil_high the high quantile to filter out for better visualization
#' @return a plot of prediction error distributed in space
#' @export


plot_error = function(validation =0, prediction  = 0 , geome, err = NA, extremequantil_low=0.025, extremequantil_high = 0.975, plotraster= T)
{
  colorB = brewer.pal(7,"Greens")
  colorG = brewer.pal(11,"PiYG")
  colorS = brewer.pal(11, "Spectral")
  spatialerr= function(validation=0, prediction=0, err=err, geome, extremequantil_low ,extremequantil_high )
  {
    if (is.na(err))
      err =  prediction-validation


    IDX= which(err > quantile(err, extremequantil_low) & err < quantile(err, extremequantil_high))

    err = err[IDX]
    geome = geome [IDX]
    sf1 = st_sf(err,geome)

    names(sf1) = c("error", "geometry")
    st_geometry(sf1) <- "geometry"
    return(sf1)

  }


  if(!is.na(err)) {
          sf1 = spatialerr(validation =validation,err=err, prediction = prediction,geome = geome, extremequantil_low =extremequantil_low,extremequantil_high =extremequantil_high)

      if (plotraster == F)
          ggplot(sf1) +
          geom_sf( aes(  color= error )) +
          scale_colour_gradientn(colours = colorG)+labs(title="validation error" )
      else {
            e <- extent(sf1)
            r <- raster(e, resolution = 0.5)
            x <- rasterize(sf1, r, fun=mean)
            rasterVis::levelplot(x[[2]], margin = F)
            }
       }
  else
  {
    sf1 = spatialerr(err=err,geome = geome,extremequantil_low =extremequantil_low ,extremequantil_high =extremequantil_high)
    if (plotraster==F)
      ggplot() +
      geom_sf(data = sf1,aes(  color= error)) +
      scale_colour_gradientn(colours = colorS)+labs(title="residuals" )+layer(sp.polygons(Germany))

    else
    {
      e <- extent(sf1)
      r <- raster(e, resolution= 0.5)
      x <- rasterize(sf1, r, fun = mean)
      rasterVis::levelplot(x[[2]], margin = F)}

  }
}
