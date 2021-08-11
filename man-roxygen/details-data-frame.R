#' @details
#' You have to consider that before tuning and fitting `x` is converted to
#' `data.frame` with a [to_data_frame()] function which converts all `character`
#' and `logical` columns to `factor` and therefore are treated as categorical
#' independent variables.
