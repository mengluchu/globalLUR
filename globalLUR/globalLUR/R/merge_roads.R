#' create a ring function
#' @param inde_var a dataframe with ROAD variables named 'ROAD', 5 types of roads, now is not a generic function, need the data to be formated in a way
#' @param classvec the vectors of road types to merge, e.g. c(1,2,3) merges road type 1,2,3.
#' @param keep indicate if the road types to be merged are to be kept, default is False, meaning not to keep them
#' @return a data frame with merged road types
#' @example merge_roads(inde_var,c(1,2))
#' @export

merge_roads = function(inde_var, classvec, keep = F) {
    a0 = 0
    for (i in 1:length(classvec)) {
        a = paste("_", classvec[i], "_", sep = "")
        r = inde_var[, which(grepl(a, names(inde_var)))]
        a0 = r + a0
        
        if (keep == F) 
            inde_var = inde_var[, -which(grepl(a, names(inde_var)))]
        
    }
    
    names(a0) = gsub(x = names(a0), pattern = paste(classvec[length(classvec)], "\\_", sep = ""), replacement = paste("M", paste(as.character(classvec), collapse = ""), "\\_", sep = ""))
    merg = cbind(a0, inde_var)
    return(merg)
}

