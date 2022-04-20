#' @return
#' An object of class `"<%= ClassName %>"` that inherits from classes
#' `"Model"` and `"R6"` with the fields:
#'
#' * `fitted_model`: An object of class [<%= refFunction %>] with the model.
#' * `x`: The final <%= XType %> used to fit the model.
#' * `y`: The final <%= YType %> used to fit the model.
#' * `hyperparams_grid`: A `data.frame` with all the computed combinations of
#'   hyperparameters and with one more column called `"loss"` with the value of
#'   the loss function for each combination. The data is ordered with the best
#'   combinations at start, sometimes with the lowest values first and other
#'   times with the greatest values first, depending the loss function.
#' * `best_hyperparams`: A `list` with the combination of hyperparameters with
#'   the best loss value (the first row in `hyperparams_grid`).
#' * `execution_time`: A `difftime` object with the total time taken to tune and
#'   fit the model.
#' * `removed_rows`: A `numeric` vector with the records' indices (in the
#'   provided position) that were deleted and not taken in account in tunning
#'   nor training.
#' * `removed_x_cols`: A `numeric` vector with the columns' indices (in the
#'   provided positions) that were deleted and not taken in account in tunning
#'   nor training.
#' * `...`: Some other parameters for internal use.
