#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include bayesian_tuner.R

DeepLearningBayesianTuner <- R6Class(
  classname = "DeepLearningBayesianTuner",
  inherit = BayesianTuner,
  public = list(
    # Methods --------------------------------------------------

    eval_one_fold = deep_learning_eval_one_fold
  )
)
