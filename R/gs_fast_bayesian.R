#' @include utils.R
#' @include validator.R
#' @include gs_fast_bayesian_cross_evaluator.R

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
