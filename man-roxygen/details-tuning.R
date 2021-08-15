#' @details
#' ## Tuning
#'
#' The hyperparameters grid to evaluate in tuning is generated with the
#' cartesian product of all the provided values (all the posible combinations)
#' in all __tunable__ parameters. If only one value of each __tunable__
#' parameter is provided no tuning is done. `tune_grid_proportion` allows you to
#' specify the proportion of all combinations you want to sample and tune, by
#' default all combinations (1) are evaluated.
#'
#' The tuning algorithm works as follows:
#'
#' ![](tuning_algorithm.png "Tuning algorithm")
