#' @importFrom R6 R6Class

#' @include model_helpers.R
#' @include bayesian_tuner.R

RadialBayesianTuner <- R6Class(
  classname = "RadialBayesianTuner",
  inherit = BayesianTuner,
  public = list(
    # Attributes --------------------------------------------------

    Pheno = NULL,
    y = NULL,
    geno_preparator = NULL,

    # Methods --------------------------------------------------

    initialize = radial_tuner_initialize,
    eval_one_fold = radial_eval_one_fold
  )
)
