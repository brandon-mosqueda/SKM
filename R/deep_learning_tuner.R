#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include metrics.R
#' @include tuner.R

DeepLearningGridTuner <- R6Class(
  classname = "DeepLearningGridTuner",
  inherit = GridTuner,
  public = list(
    # Methods --------------------------------------------------

    eval_one_fold = function(fold, combination) {
      x_training <- get_records(self$x, fold$training)
      y_training <- get_records(self$y, fold$training)
      x_testing <- get_records(self$x, fold$testing)
      y_testing <- get_records(self$y, fold$testing)

      model <- self$training_function(
        x = x_training,
        y = y_training,
        hyperparams = combination,
        other_params = self$other_params,
        x_testing = x_testing,
        y_testing = y_testing
      )

      return(model$validation_loss)
    }
  )
)
