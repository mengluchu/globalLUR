#' error matrix outputs RMSE, MAE, IQR
#' @param validataion The validataion vector
#' @param prediction The prediction vector
#' @return an errormatrix with RMSE, MAE, IQR
#' @export


error_matrix = function(validation, prediction)
{
  rmse <- function(test,pred)
  {
    RMSE= sqrt(mean( (pred - test)^2))
    RMSE
    # x= pred - test
    #  RMSE <-  sqrt(sum(x^2) /length(x))
  }
  MAE = function(test, pred)
  {
    mae = mean(abs(pred-test))
    mae
  }
  IQR <- function(test, pred)
  {
    a2 = summary(as.vector(pred-test))
    iqr_error= as.vector(a2[5]- a2[2])
    iqr_error
  }
  rmse1 = rmse(validation, prediction)

  MAE1=MAE(validation, prediction)

  IQR1 = IQR(validation, prediction)
  c(rmse = rmse1, MAE=MAE1, IQR=IQR1)
}
