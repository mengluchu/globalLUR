#'devide the data into test and training set, and dataframe for modeling
#' @param  originaldata original dataframe
#' @param  fraction, fraction for the training
#' @param  country2digit country code, 2 digit, if NA or not in the database, the world is returned, the sampling is equal fraction per country
#' @return inde_var the matrix containing response and predictors.  NA values are removed, and columes with all values 0 or less than 0 are removed
#'
#' @examples
#' a = sampledf(merged, fraction = 0.8, "ES")
#' test = a$test
#' training = a$training
#' inde_var = a$indevar
#' @export
#'
#require(tidyr)
#require(dplyr)
#require(sf)
#require(RColorBrewer)
sampledf = function(originaldata,fraction=0.8, country2digit=NA ){

  buffer_oq_dense = na.omit(originaldata)

  if (country2digit%in% buffer_oq_dense$country )
{
  buffer_oq_dense <- buffer_oq_dense%>% filter(country==country2digit)
  buffer_oq_dense$IDnew= 1: nrow(buffer_oq_dense)
  trainingdf = buffer_oq_dense%>% sample_frac(fraction) # use a fraction for training
  }
else
  {
    warning("country not in the database, return all the countries, sampling based on fraction per country, for world mapping")
    buffer_oq_dense$IDnew= 1: nrow(buffer_oq_dense)
    # sample with a fraction by country for the world
    trainingdf = buffer_oq_dense%>% sample_frac(fraction) %>% group_by(country)
  }
# index
training = trainingdf$IDnew
test = buffer_oq_dense$IDnew[-training]
inde_var = buffer_oq_dense[, -which(grepl("ID|LATITUDE|LONGITUDE|ROAD_0|geometry|countryfullname", names(buffer_oq_dense)))]

#prex_f = model.matrix(value_mean~., inde_var)  #  only to get the model matrix

rm0 = which(apply(inde_var, 2, max)==0)
if (length(rm0)!=0)
  inde_var = inde_var[,-rm0]

return(list(training =training, test =test,inde_var=inde_var))
}




