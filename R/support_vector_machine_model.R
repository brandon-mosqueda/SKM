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

      self$hyperparams$degree <- nonull(
        prepare_degree(kernel, degree),
        degree[1]
      )
      self$hyperparams$gamma <- nonull(
        prepare_gamma(kernel, gamma),
        gamma[1]
      )
      self$hyperparams$coef0 <- nonull(
        prepare_coef0(kernel, coef0),
        coef0[1]
      )
      self$hyperparams$cost <- cost

      self$other_params$scale <- scale
      self$other_params$kernel <- tolower(kernel)
      if (is.character(class_weights)) {
        class_weights <- tolower(class_weights)
      }
      self$other_params$class_weights <- class_weights
      self$other_params$cache_size <- cache_size
      self$other_params$tolerance <- tolerance
      self$other_params$epsilon <- epsilon
      self$other_params$shrinking <- shrinking
      self$other_params$fitted <- fitted
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_others = function() {
      # Remove the scale values if there was records deleted
      self$other_params$scale <- remove_if_has_more(
        self$other_params$scale,
        ncol(self$x),
        self$removed_x_cols
      )
    },

    train_univariate = function(x, y, hyperparams, other_params) {
      model <- svm(
        x = x,
        y = y,

        degree = hyperparams$degree,
        gamma = hyperparams$gamma,
        coef0 = hyperparams$coef0,
        cost = hyperparams$cost,

        scale = other_params$scale,
        kernel = other_params$kernel,
        class_weights = other_params$class_weights,
        cache_size = other_params$cache_size,
        tolerance = other_params$tolerance,
        epsilon = other_params$epsilon,
        shrinking = other_params$shrinking,
        fitted = other_params$fitted,

        probability = TRUE
      )

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  other_params,
                                  hyperparams) {
      predictions <- predict(model, x, probability = TRUE)

      if (is_class_response(responses$y$type)) {
        probabilities <- attr(predictions, "probabilities")
        attr(predictions, "probabilities") <- NULL
        names(predictions) <- NULL

        predictions <- list(
          predicted = predictions,
          probabilities = as.data.frame(probabilities[, responses$y$levels])
        )
      } else {
        predictions <- list(predicted = predictions)
      }

      return(predictions)
    }
  )
)
