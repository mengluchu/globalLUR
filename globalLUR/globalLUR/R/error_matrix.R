#' error matrix outputs RMSE, MAE, IQR
#' @param validataion The validataion vector
#' @param prediction The prediction vector
#' @return an errormatrix with RMSE, MAE, IQR
#' @export


error_matrix = function(validation, prediction) {
    rmse <- function(test, pred) {
         sqrt(mean((pred - test)^2))
    }
    MAE = function(test, pred) {
       mean(abs(pred - test))

    }
    IQR <- function(test, pred) {
        a2 = summary(as.vector(pred - test))
        as.vector(a2[5] - a2[2])

    }
    rIQR <- function(test, pred) {
        a2 = summary(as.vector(pred - test))
        as.vector(a2[5] - a2[2])/median(test)
          # divided by median
    }
    Rsquare <- function(test, pred) {
        1-  var(pred - test)/var(test)
    }

    rrmse <- function(test, pred) {
         rmse = sqrt(mean((pred - test)^2))
         rmse/mean(test)
    }
    rMAE = function(test, pred) {
        mean(abs(pred - test))/mean(test)
    }
    rmse1 = rmse(validation, prediction)
    rrmse1 = rrmse(validation, prediction)

    MAE1 = MAE(validation, prediction)
    rMAE1 = rMAE(validation, prediction)
    IQR1 = IQR(validation, prediction)
    rIQR1 = rIQR(validation, prediction)

    rsqd1 = Rsquare(validation, prediction)
    c(RMSE = rmse1, RRMSE = rrmse1, IQR = IQR1, rIQR = rIQR1, MAE=MAE1, rMAE = rMAE1, rsq = rsqd1)
}
