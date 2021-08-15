#' @description
#' All the parameters marked as (__tunable__) accept a vector of values with
#' wich the grid is generated for tuning. The returned object contains a
#' `data.frame` with the hyperparameter grid. In the end the best combination of
#' hyperparameters is used to fit the final model, which is also returned and
#' can be used to make new predictions.
