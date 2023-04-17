#' @include utils.R
#' @include validator.R
#' @include gs_fast_bayesian_cross_evaluator.R

#' @title Fast Bayesian Cross Validation
#'
#' @description
#' This function performs a cross validation using the Fast Bayesian method
#' which requires to fit only one model per trait.
#'
#' @param Pheno (`data.frame`) The phenotypic data. `Env` and `Line` columns
#'   are required.
#' @param Geno (`matrix`) The genotypic data. It must be a square matrix with
#'   the row and column names set to the line names in `Pheno`.
#' @param traits (`character`) The columns' names in `Pheno` to be used as
#'   traits.
#' @param folds (`list`) A list of folds. Each fold is a named list with two
#'   entries: `training`, with a vector of indices for training set, and
#'   `testing`, with a vector of indices for testing set. Note that this is
#'   default format for `cv_*` functions of SKM libraries.
#' @param is_multitrait (`logical(1)`) Is multitrait analysis? `FALSE` by
#'   default.
#' @param iterations_number (`numeric(1)`) Number of iterations to fit the
#'   model. 1500 by default.
#' @param burn_in (`numeric(1)`) Number of items to burn at the beginning of the
#'   model. 500 by default.
#' @param thinning (`numeric(1)`) Number of items to thin the model. 5 by
#'   default.
#' @template other-base-params
#'
#' @return
#' A `GSFastBayesian` object with the following attributes:
#'   * Pheno: (`data.frame`) The phenotypic data.
#'   * Geno: (`matrix`) The genotypic data.
#'   * traits: (`character`) The traits' names.
#'   * is_multitrait: (`logical(1)`) Is multitrait analysis?
#'   * Predictions: (`data.frame`) The predictions of cross validation. This
#'       `data.frame` contains the `Trait`, `Fold`, `Line`, `Env`, `Predicted`
#'       and `Observed` columns.
#'   * execution_time: (`difftime`) The execution time taken for the analysis.
#'   * folds: (`list`) The folds used in the analysis.
#'   * model: (`BayesianModel`) The model fitted.
#'   * model_name: (`character(1)`) The name of the model.
#'   * iterations_number: (`numeric(1)`) Number of iterations to fit the model.
#'   * burn_in: (`numeric(1)`) Number of items to burn at the beginning of the
#'       model.
#'   * thinning: (`numeric(1)`) Number of items to thin the model.
#'
#' @family gs_models
#'
#' @example inst/examples/gs_fast_bayesian.R
#'
#' @export
gs_fast_bayesian <- function(Pheno,
                             Geno,
                             traits,
                             folds,

                             is_multitrait = FALSE,

                             iterations_number = 1500,
                             burn_in = 500,
                             thinning = 5,

                             seed = NULL,
                             verbose = TRUE) {
  predictors <- c("Env", "Line", "EnvxLine")
  model <- "BGBLUP"

  validate_gs_fast_bayesian(
    Pheno = Pheno,
    Geno = Geno,
    traits = traits,
    folds = folds,

    model = model,
    predictors = predictors,
    is_multitrait = is_multitrait,

    seed = seed,
    verbose = verbose
  )

  old_random_state <- NULL
  if (!is.null(seed)) {
    old_random_state <- get_rand_state()

    set.seed(seed)
  }
  on.exit(set_rand_state(old_random_state))

  start_time <- Sys.time()

  model <- GSFastBayesianCrossEvaluator$new(
    Pheno = Pheno,
    Geno = Geno,
    traits = traits,
    model = model,
    is_multitrait = is_multitrait,
    predictors = predictors,
    folds = folds,
    iterations_number = iterations_number,
    burn_in = burn_in,
    thinning = thinning
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$eval())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model$export())
}
