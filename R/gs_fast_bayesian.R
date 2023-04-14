gs_fast_bayesian <- function(Pheno,
                             Geno,
                             traits,
                             folds,

                             is_multivariate = FALSE,
                             predictors = list(Line = "BGBLUP"),

                             iterations_number = 1500,
                             burn_in = 500,
                             thinning = 5,

                             seed = NULL,
                             verbose = TRUE) {
  validate_gs_fast_bayesian(
    Pheno = Pheno,
    Geno = Geno,
    traits = traits,

    is_multivariate = is_multivariate,
    predictors = predictors,
    folds = folds,

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

  model <- GSFastBayesian$new(
    Pheno = Pheno,
    Geno = Geno,
    traits = traits,
    is_multivariate = is_multivariate,
    predictors = predictors,
    iterations_number = iterations_number,
    burn_in = burn_in,
    thinning = thinning,
    verbose = verbose
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
