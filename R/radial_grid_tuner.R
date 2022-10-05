#' @importFrom R6 R6Class

#' @include model_helpers.R
#' @include grid_tuner.R

RadialGridTuner <- R6Class(
  classname = "RadialGridTuner",
  inherit = GridTuner,
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
