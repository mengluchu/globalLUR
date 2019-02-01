#' create a ring function
#' @param inde_var a dataframe with ROAD variables named "ROAD", n types of roads, now it is not a very generic function, need the data to be formated in a way as the buffers are ordered (e.g. 25m, 50m, 100m)
#' @param normalize if T, devide the ring by the area of ring. Default is faulse
#' @param buffers_in buffer sizes of the inner rings
#' @param buffer_out buufer sizes of the outter rings
#' @param Road_varname the name of the road variable, grepl styple e.g. "ROAD"
#' @return the road buffers becomes road ring buffers
#' @export

create_ring = function(inde_var,normalize=F, buffers_in = c(0,25,50,100,300,500,800, 1000,3000),
                       buffers_out = c(25, 50,100,300,500, 800,1000,3000,5000),number_roadtypes, Road_varname = "ROAD")

{
  buffernumber = length(buffers_in)
  roadsonly =  subset_grep(inde_var, Road_varname)
 # for5000m = seq(buffernumber,  number_roadtypes*buffernumber, by = buffernumber)
 # roadsonly_in = roadsonly
 # roadsonly_in[,for5000m] = 0 # don't need 5000, so let them be 0 and put them in front, for road ring 0.
  othervar = inde_var[, -which(grepl(Road_varname, names( inde_var) ))]
  #if(is.null(names(othervar)))
  #names(othervar) = "VAR1"
  #roadsonlyin_s = roadsonly_in [,c(7,1:6,14, 8:13, 21, 15:20, 28, 22:27)] #shuffel, 0 is in the beginning, for buffer1
 # roadsonlyin_s = roadsonly_in [,c(for5000m[1],1:6,for5000m[2], 8:13, for5000m[3], 15:20 )]
  #roadsonlyin_s = roadsonly_in [,c(7,1:6,14, 8:13, 21, 15:20, 28, 22:27,35, 29:34)]
  # roadrings = roadsonly - roadsonlyin_s
  getring=function(i, roadsonly, bf = buffernumber)
   {
    roadtype_n = roadsonly[,((i-1)*bf+1) :(i*bf)]

   roadring_n =  cbind(roadtype_n[,1],t(apply( roadtype_n, 1, diff)))
 }
 roadr= data.frame(lapply(1:number_roadtypes, getring, roadsonly))

  ringnames = paste(names(roadsonly) , buffers_in,sep = "_")
  names(roadr ) = ringnames

   roadrings = data.frame(cbind(othervar, roadr ))


  if (normalize )
  {
    area = function(r)  pi*(r^2)

    arearing = sapply(buffers_out, area)- sapply(buffers_in, area)

    nor_ring = sweep(roadrings,2, t(rep(arearing,5)), "/")
  }

  return(roadrings)
  }
#devtools::create("globalLUR")
#load_all()
#document()
