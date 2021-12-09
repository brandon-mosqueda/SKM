#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include grid_tuner.R

DeepLearningGridTuner <- R6Class(
  classname = "DeepLearningGridTuner",
  inherit = GridTuner,
  public = list(
    # Methods --------------------------------------------------

    eval_one_fold = deep_learning_eval_one_fold
  )
)
