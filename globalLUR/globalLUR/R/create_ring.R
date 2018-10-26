#' create a ring function
#' @param inde_var a dataframe with ROAD variables named "ROAD", 5 types of roads, now is not a generic function, need the data to be formated in a way
#' @param normalize if T, devide the ring by the area of ring. Default is faulse
#' @return the road buffers becomes road ring buffers
#' @export

create_ring = function(inde_var,normalize=F)
{
  roadsonly =  inde_var[, which(grepl("ROAD",dimnames( inde_var)[[2]]))]
  for5000m = seq(7, 35, by = 7)
  roadsonly_in = roadsonly
  roadsonly_in[,for5000m] = 0 # don't need 5000, so let them be 0 and put them in front, for road ring 0.
  othervar = inde_var[, -which(grepl("ROAD",dimnames( inde_var)[[2]]))]
  #if(is.null(names(othervar)))
  #names(othervar) = "VAR1"
  #roadsonlyin_s = roadsonly_in [,c(7,1:6,14, 8:13, 21, 15:20, 28, 22:27)] #shuffel, 0 is in the beginning, for buffer1

  roadsonlyin_s = roadsonly_in [,c(7,1:6,14, 8:13, 21, 15:20, 28, 22:27,35, 29:34)]

  buffers1 = c(0,25,50,100,300,500,1000)
  buffers2 = c(25, 50,100,300,500,1000,5000)
  roadrings = roadsonly - roadsonlyin_s
  ringnames = paste(dimnames(roadrings)[[2]], buffers1,sep = "_")
  dimnames(roadrings)[[2]] = ringnames
  roadrings = data.frame(cbind(othervar, roadrings))


  if (normalize )
  {
    area = function(r)  pi*(r^2)

    arearing = sapply(buffers2, area)- sapply(buffers1, area)

    nor_ring = sweep(roadrings,2, t(rep(arearing,5)), "/")
  }

  return(roadrings)
  }
#devtools::create("globalLUR")
#load_all()
#document()
