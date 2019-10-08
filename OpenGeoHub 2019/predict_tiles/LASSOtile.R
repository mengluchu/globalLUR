
ipak <- function(pkg){

  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.muenster.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}
packages <- c( "raster", "dplyr", "glmnet","rgdal"  )
ipak(packages)


#library(globalLUR)
#data(merged)
#if not loading the globalLUR package
source("predict_tiles/GlobalLUR_R/sampledf.R")
source("predict_tiles/GlobalLUR_R/subset_grep.R")
source("predict_tiles/GlobalLUR_R/merge_roads.R")
load("predict_tiles/data/mergedu.Rdata")
#

COUN =  "de"
a = sampledf(merged,fraction = 1, country2digit = toupper(COUN))
#a=  sampledf(merged,fraction = 1, country2digit = "World") #for world

inde_var = a$inde_var

# rename so that the variable names and the raster names match
  names(inde_var) =  gsub("ROAD_", "road_class_", names(inde_var))
  names(inde_var) =  gsub("I_1", "industry", names(inde_var))
  names(inde_var) =  gsub("Tropomi_2018", "trop_mean_filt", names(inde_var))
  names(inde_var) =  gsub("RSp", "Rsp", names(inde_var))

# replace -1 into na
  merged_1 = na_if(merged,-1)

# this is the key to apply all the methods to raster  
  predfun <- function(model, data) {
    v <- predict(model, as.matrix(data ))
  }


# merge raster roads
    lf  = list.files(paste0("predict_tiles/", toupper(COUN), "/wgs84"), pattern = "*.tif$", full.names = T)
    lf  = lf[-which(grepl("road_class_3|road_class_4|road_class_5", lf))]
    lfr3  = list.files(paste0("predict_tiles/", toupper(COUN), "/wgs84"), pattern = "road_class_3_.*.tif$", full.names = T)
    sr3 = stack(lfr3)
    lfr4  = list.files(paste0("predict_tiles/", toupper(COUN), "/wgs84"), pattern = "road_class_4_.*.tif$", full.names = T)
    sr4 = stack(lfr4)
    lfr5  = list.files(paste0("predict_tiles/", toupper(COUN), "/wgs84"), pattern = "road_class_5_.*.tif$", full.names = T)
    sr5 = stack(lfr5)
    sr345 = sr3 + sr4 + sr5 # aggregate roads 345 and the name is 3
    
    
#merge roads
    inde_var = merge_roads(inde_var ,c(3, 4, 5), keep = F)
#match names
    names(sr345) = names(subset_grep(inde_var, "road_class_M345"))
    s1 = stack(lf, sr345) 

# select variables
    pre_mat3 = subset_grep(inde_var, "road|temperature|wind|pop|ele|Rsp|rop|OMI|industry")
# reorder the dataframe! seems lasso only match the matrix
    pre_mat3 %>% select (names(s1)) -> pre_mat3
# make sure the nams match!   
     all.equal(names(s1), names(pre_mat3))
    
     L_day <- glmnet::cv.glmnet(as.matrix(pre_mat3), inde_var$day_value, type.measure = "mse", standardize = TRUE, alpha = 1, lower.limit = 0)

    L_night <- glmnet::cv.glmnet(as.matrix(pre_mat3), inde_var$night_value, type.measure = "mse", standardize = TRUE, alpha = 1, lower.limit = 0)



    sday = predict(s1, L_day, fun = predfun)
    snight = predict(s1, L_night, fun = predfun)

    plot(sday)
    plot(snight)
    dirout1 = paste0("predict_tiles/output/")
    name1 = paste0( dirout1,  COUN , "Laday.tif")

    name2 = paste0( dirout1,  COUN , "Lanight.tif")
    writeRaster(sday, name1, overwrite = TRUE )
    writeRaster(snight, name2, overwrite = TRUE )


