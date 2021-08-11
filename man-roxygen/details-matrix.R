#' @details
#' You have to consider that before tuning and fitting `x` is converted to
#' `matrix` with a [to_matrix()] function and all columns without variance
#' (where all the records has the same value) are removed. Such columns
#' positions are returned in the `removed_x_cols` field of the returned object.
