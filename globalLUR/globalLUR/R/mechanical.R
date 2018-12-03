#' mechanical model: use an exponential function to describe the coefficients away from roads
#' @param  variabledf the dataframe containing predictors and dependent variable
#' @param y_var  name of the dependent variable.
#' @param distance_centre the distance to centre from each buffer.  (b2-b1)/2 + b1
#' @param pop_var the name of an additional variable as a linear term,  usually population withn a buffer, a string.
#'  @param Road_varname the name of variables contains road buffers, e.g. "ROAD"
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param nls2start the start value for nls2. if providing an nls2start, the nls2 from nls2 package is used. Details see nls2.
#' @param normalize if True, the road ring is normalised by the area (default is false)

#' @details This method used nls for modelling. This function also prints errormatrix, the exponential model; plot coefficient. The modelling and evaluation should be separated at a later stage, now putting together for exploration only.
#' @return An object of nls
#' @export
#' @example mechanical(inde_var,"day_value",pop_var = "pop5k", distance_centre = distance_centre, training, test)

mechanical= function(variabledf, y_var=c("day_value","night_value", "value_mean"), pop_var="pop3k", distance_centre, training, test,nls2start=NA,Road_varname = "ROAD",normalize=F)
{

  variabledf_tr = variabledf[training,]

  roadsonly =  variabledf[, which(grepl(Road_varname,
                                        names(variabledf)))] # 25 -50000

  ringsonly = create_ring(roadsonly,normalize=normalize, number_roadtypes = 3)

  ringsonly_tr = ringsonly[training,]


  b = paste0("Q",rep(1:3, each = 7))
  disl = paste("dis", 1:7, sep = "")
  formu1 = as.formula(paste("y_train~",
                            paste(  names( ringsonly)  , "*", b,"*exp( a *", disl  ,")",collapse = "+"), "+d*pop", "+c"))


  # form the dataframe
  disdf = data.frame(mapply(rep,distance_center, each = nrow(ringsonly_tr) ))


  names(disdf)= disl

  rdf = cbind(ringsonly_tr,disdf, y_train=variabledf_tr[,y_var],RS = variabledf_tr$RSp,pop  = variabledf_tr[,pop_var])

  #rdf = cbind(rdf, OMI_mean_filt =data.frame(xtrain_f)$OMI  )

  # model: y = roadring * exp ( dis * a)

 if (!is.na(nls2start))
  a1 =  nls2(formu1, data = rdf,start = nls2start)

 else
   a1 =  nls(formu1, data = rdf,start =  list(a = -0.001,Q1=0.01, Q2 = 0.001,Q3 = 0.0001, c = 0.001, d = 0.001))

  print(summary(a1))
  variabledf_test = variabledf[test,]
  ringsonly_test =ringsonly[test,]

  disdf2= data.frame(mapply(rep,distance_center, each = nrow(ringsonly_test) ))
  names(disdf2)= disl

  rd2 = cbind(ringsonly_test , disdf2,pop  = variabledf_test[,pop_var])

  dp = predict(a1,  newdata = rd2)
  y_test_day = variabledf[test,y_var]
  print(error_matrix( y_test_day,dp ))

  coef = coefficients(a1)
  coef
  plot(y_test_day,typ = "line")
  lines(dp , col= "red" )

  plot( distance_center ,coef[2]*exp(distance_center *coef[1]), typ = "l" , main = "coefficients", ylim=c(0,8e-03) ,col = colorG[1])
  lines( distance_center ,coef[3]*exp(distance_center *coef[1]), typ = "l" , main = "coefficients",col = colorG[3])
  lines( distance_center ,coef[4]*exp(distance_center *coef[1]), typ = "l" , main = "coefficients",col = colorG[4])

  legend("topright", c("r1","r2","r3" ), lty = 1, col= colorG[c(1,3,4)])
  return(a1)
}
