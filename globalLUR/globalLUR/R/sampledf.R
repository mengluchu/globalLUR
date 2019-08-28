#'devide the data into test and training set, and prepare the dataframe for modeling
#' @param  originaldata original dataframe
#' @param  fraction, fraction for the training
#' @param  country2digit country code, 2 digit, if NA or not in the database, the world is returned, the sampling is equal fraction per country
#' @param  grepstring_rm the variables that are to be removed, grepl style, e.g. 'ID|LATITUDE|LONGITUDE|ROAD_0|geometry|countryfullname'
#' @param rm_neg_col if True, the columes containing all negative values or 0s are removed
#' @return inde_var the matrix containing response and predictors. Default is false.
#' @details NA values are removed. This function is used for preprocessing, can be improved.
#'
#' @examples
#' a = sampledf(merged, fraction = 0.8, 'ES')
#' test = a$test
#' training = a$training
#' inde_var = a$indevar
#' @export
#'
# require(tidyr) require(dplyr) require(sf) require(RColorBrewer)
sampledf = function(originaldata, fraction = 0.8, country2digit = NA, grepstring_rm = "ID|LATITUDE|LONGITUDE|ROAD_0|geometry|countryfullname", rm_neg_col = F) {
    buffer_oq_dense = na.omit(originaldata)
    if (country2digit %in% buffer_oq_dense$country) {
        buffer_oq_dense = buffer_oq_dense %>% filter(country == country2digit)
        buffer_oq_dense$IDnew = 1:nrow(buffer_oq_dense)
        trainingdf = buffer_oq_dense %>% sample_frac(fraction)  # use a fraction for training
    } else {
        warning("country not in the database, return all the countries, sampling based on fraction per country, for world mapping")
        buffer_oq_dense$IDnew = 1:nrow(buffer_oq_dense)
        # sample with a fraction by country for the world
        trainingdf = buffer_oq_dense %>% sample_frac(fraction) %>% group_by(country)
    }
    # index
    training = trainingdf$IDnew
    test = buffer_oq_dense$IDnew[-training]
    inde_var = buffer_oq_dense[, -which(grepl(grepstring_rm, names(buffer_oq_dense)))]
    
    # prex_f = model.matrix(value_mean~., inde_var) # only to get the model matrix
    if (rm_neg_col) {
        rm0 = which(apply(inde_var, 2, max) == 0)
        if (length(rm0) != 0) 
            inde_var = inde_var[, -rm0]
    }
    return(list(training = training, test = test, inde_var = inde_var))
}




