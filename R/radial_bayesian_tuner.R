#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include bayesian_tuner.R

RadialBayesianTuner <- R6Class(
  classname = "RadialBayesianTuner",
  inherit = BayesianTuner,
  public = list(
    # Attributes --------------------------------------------------

    Pheno = NULL,
    y = NULL,
    predictors = NULL,
    geno_preparator = NULL,
    model_iterations_number = NULL,
    burn_in = NULL,
    thinning = NULL,

    # Methods --------------------------------------------------

    initialize = radial_tuner_initialize,
    eval_one_fold = radial_eval_one_fold
  )
)
