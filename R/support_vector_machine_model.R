#' @importFrom R6 R6Class
#' @importFrom e1071 svm

#' @include utils.R
#' @include model.R
#' @include model_helpers.R

SupportVectorMachineModel <- R6Class(
  classname = "SupportVectorMachineModel",
  inherit = Model,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(...,
                          kernel,

                          degree,
                          gamma,
                          coef0,
                          cost,

                          scale,
                          class_weights,
                          cache_size,
                          tolerance,
                          epsilon,
                          shrinking,
                          fitted,
                          na_action) {
      super$initialize(
        ...,
        name = "Support Vector Machine",
        is_multivariate = FALSE
      )

      self$fit_params$degree <- nonull(
        prepare_degree(kernel, degree),
        degree[1]
      )
      self$fit_params$gamma <- nonull(
        prepare_gamma(kernel, gamma),
        gamma[1]
      )
      self$fit_params$coef0 <- nonull(
        prepare_coef0(kernel, coef0),
        coef0[1]
      )
      self$fit_params$cost <- cost

      self$fit_params$scale <- scale
      self$fit_params$kernel <- tolower(kernel)
      if (is.character(class_weights)) {
        class_weights <- tolower(class_weights)
      }
      self$fit_params$class_weights <- class_weights
      self$fit_params$cache_size <- cache_size
      self$fit_params$tolerance <- tolerance
      self$fit_params$epsilon <- epsilon
      self$fit_params$shrinking <- shrinking
      self$fit_params$fitted <- fitted
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_others = function() {
      # Remove the scale values if there was records deleted
      self$fit_params$scale <- remove_if_has_more(
        self$fit_params$scale,
        ncol(self$x),
        self$removed_x_cols
      )

      if (is_bayesian_tuner(self$tuner_class)) {
        self$fit_params$degree <- format_bayes_hyperparam(
          self$fit_params$degree,
          is_int = TRUE
        )
        self$fit_params$gamma <- format_bayes_hyperparam(
          self$fit_params$gamma
        )
        self$fit_params$coef0 <- format_bayes_hyperparam(
          self$fit_params$coef0
        )
        self$fit_params$cost <- format_bayes_hyperparam(
          self$fit_params$cost
        )
      }
    },

    train_univariate = function(x, y, fit_params) {
      model <- svm(
        x = x,
        y = y,

        degree = fit_params$degree,
        gamma = fit_params$gamma,
        coef0 = fit_params$coef0,
        cost = fit_params$cost,

        scale = fit_params$scale,
        kernel = fit_params$kernel,
        class_weights = fit_params$class_weights,
        cache_size = fit_params$cache_size,
        tolerance = fit_params$tolerance,
        epsilon = fit_params$epsilon,
        shrinking = fit_params$shrinking,
        fitted = fit_params$fitted,

        probability = TRUE
      )

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  fit_params) {
      predictions <- predict(model, x, probability = TRUE)

      if (is_class_response(responses$y$type)) {
        probabilities <- attr(predictions, "probabilities")
        probabilities <- as.data.frame(probabilities[, responses$y$levels])

        predictions <- predict_class(probabilities, responses$y)
      } else {
        predictions <- list(predicted = as.numeric(predictions))
      }

      return(predictions)
    }
  )
)
