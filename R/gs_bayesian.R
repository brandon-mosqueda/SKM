#' @include utils.R
#' @include validator.R
#' @include gs_bayesian_cross_evaluator.R

#' @title Bayesian Cross Validation for Genomic Selection
#'
#' @inherit gs_fast_bayesian
#'
#' @description
#' This function performs a cross validation using the Bayesian models.
#'
#' @param model (`character`) (case not sensitive) The model to be used. It
#'   supports the same model as [SKM::bayesian_model], that is, `"BGBLUP"`,
#'   `"BRR"`, `"Bayes_Lasso"`, `"Bayes_A"`, `"Bayes_B"` or `"Bayes_C"`.
#'   In multivariate analysis you can only use `"BGBLUP"` or `"BRR"`. `"BGBLUP"`
#'   by default.
#' @param predictors (`character`) (case not sensitive) The predictors to be
#'   used in the model. At least one of the following options: "Env" for the
#'   environment effect, "Line" for the line effect and "EnvxLine" for the
#'   interaction between environment and line. `c("Env", "Line", "EnvxLine")`
#'   by default.
#'
#' @family gs_models
#'
#' @example inst/examples/gs_bayesian.R
#'
#' @export
gs_bayesian <- function(Pheno,
                        Geno,
                        traits,
                        folds,

                        model = "BGBLUP",
                        predictors = c("Env", "Line", "EnvxLine"),
                        is_multitrait = FALSE,

                        iterations_number = 1500,
                        burn_in = 500,
                        thinning = 5,

                        seed = NULL,
                        verbose = TRUE) {
  validate_gs_fast_bayesian(
    Pheno = Pheno,
    Geno = Geno,
    traits = traits,
    folds = folds,

    model = model,
    predictors = predictors,
    required_predictors = NULL,
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

  model <- GSBayesianCrossEvaluator$new(
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
