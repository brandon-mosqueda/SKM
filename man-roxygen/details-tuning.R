#' @details
#' ## Tuning
#'
#' The general tuning algorithm works as follows:
#'
#' ![](tuning_algorithm.png "Tuning algorithm")
#'
#' For grid search tuning, the hyperparameters grid is generated (step one in
#' the algorithm) with the cartesian product of all the provided values (all the
#' posible combinations) in all __tunable__ parameters. If only one value of
#' each __tunable__ parameter is provided no tuning is done.
#' `tune_grid_proportion` allows you to specify the proportion of all
#' combinations you want to sample from the full grid and tune them, by default
#' all combinations are evaluated.
#'
#' For bayesian optimization tuning, step one in the algorithm works a little
#' different. At start, `tune_bayes_samples_number` different
#' hyperparameters combinations are generated and evaluated, then
#' `tune_bayes_iterations_number` new hyperparameters combinations are generated
#' and evaluated iteratively based on the bayesian optimization algorithm, but
#' this process is equivalent to that described in the general tuninig
#' algorithm. Note that only the hyperparameters for which the list of min and
#' max values were provided are tuned and their values fall in the specified
#' boundaries.
