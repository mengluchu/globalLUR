# globalLUR
##tools for LUR project
#### Contains an R package globalLUR, which includes the following functions:
* Preprocessing: adding variable, merging road types, separate training and testing, prepare dataframe for modeling
⋅⋅*  join_by_id          
⋅⋅*  mergeraster2file
⋅⋅*  merge_roads      
⋅⋅*  sampledf 
     
 * Data exploration: Scatter plot, inspecting and comparing correlations between variables.
⋅⋅* plot_rsq
⋅⋅* RDring_coef         
⋅⋅* scatterplot             
⋅⋅* univar_rsq      

 * Modelling and validation: producing plots and error matrices as results of different modeling: LM, Random forest, Lasso, mechanical model.
⋅⋅* create_ring          
⋅⋅* error_matrix 
⋅⋅* Lasso                
⋅⋅* Lassoselected
⋅⋅* mechanical
⋅⋅* plot_error          
#### The daynight_clearn_DE provide examples of how the functions are used, and reproduces the modeling process
