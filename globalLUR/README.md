
### GlobalLUR is the R package for auxilary functions used in the project "Statistical method of global air pollution modeling"
  
```
install_github("mengluchu/globalLUR/globalLUR/globalLUR") 
library(globalLUR)
ls("package:globalLUR")
```

##### Preprocessing: 
Adding variable, merging road types, separate training and testing, prepare dataframe for modeling

*  join_by_id          
*  mergeraster2file
*  merge_roads      
*  sampledf 
     
##### Data exploration: 
Scatter plot, inspecting and comparing correlations between variables.

* plot_rsq
* RDring_coef         
* scatterplot             
* univar_rsq      

##### Modelling and validation: 
Plotting and error matrices as results of different modeling: LM, Random forest, Lasso, mechanical model. Wrapping functions created for calculating error matrix and importance variables, when applicable.  
* create_ring          
* error_matrix 
* Lasso                
* Lassoselected
* mechanical
* plot_error          
* xboost_.., Brt_.., rf_.., cforest_.., ctree_.. 


##### General functions:
- removedips

remove stations that have time series points less than a certain percentage (by default 15%).

- error_matrix 

calculating RMSE, MAE, IQR
  
- subset_grep

Subsetting a dataframe using grep style strings.

- ppm2ug 

converting NO2 from ppm to ug/m3
 
- sampledf

creating training and test dataset, and remove the variables that are not needed. 

- mergeraster2file

merge raster file to the dataframe


- Mechanical
a novel method developed for air pollution modeling with an attempt to integrating air distribution mechanisms. 

 
