#' @importFrom R6 R6Class
#' @importFrom e1071 svm

#' @include model.R

SVMModel <- R6Class(
  classname = "SVMModel",
  inherit = Model,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(...,
                          svm_kernel,
                          svm_degree,
                          svm_gamma,
                          svm_coef0,
                          cost,

                          scale,
                          class_weights,
                          cache_size,
                          tolerance,
                          epsilon,
                          shrinking,
                          fitted,
                          na_action) {
      super$initialize(..., name = "SVM", is_multivariate = FALSE)

      self$hyperparams$svm_degree <- nonull(
        prepare_degree(svm_kernel, svm_degree),
        svm_degree[1]
      )
      self$hyperparams$svm_gamma <- nonull(
        prepare_gamma(svm_kernel, svm_gamma),
        svm_gamma[1]
      )
      self$hyperparams$svm_coef0 <- nonull(
        prepare_coef0(svm_kernel, svm_coef0),
        svm_coef0[1]
      )
      self$hyperparams$cost <- cost

      if (!is.null(self$other_params$kernel) &&
          !has_str(svm_kernel, "linear")) {
        warning(
          "svm_kernel changed to {'linear'} due to you are using {",
          set_collapse(self$other_params$kernel),
          "} kernel"
        )
        svm_kernel <- "linear"
      }

      self$other_params$scale <- scale
      self$other_params$svm_kernel <- svm_kernel
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

    train_univariate = function(x, y, hyperparams, other_params) {
      model <- svm(
        x = x,
        y = y,

        degree = hyperparams$svm_degree,
        gamma = hyperparams$svm_gamma,
        coef0 = hyperparams$svm_coef0,
        cost = hyperparams$cost,

        scale = other_params$scale,
        kernel = other_params$svm_kernel,
        class_weights = other_params$class_weights,
        cache_size = other_params$cache_size,
        tolerance = other_params$tolerance,
        epsilon = other_params$epsilon,
        shrinking = other_params$shrinking,
        probability = TRUE,
        fitted = other_params$fitted
      )

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  is_multivariate,
                                  other_params,
                                  hyperparams) {
      predictions <- predict(model, x, probability = TRUE)

      if (is_class_response(responses[["y"]]$type)) {
        probabilities <- attr(predictions, "probabilities")
        attr(predictions, "probabilities") <- NULL
        names(predictions) <- NULL

        predictions <- list(
          predicted = predictions,
          probabilities = probabilities
        )
      } else {
        predictions <- list(predicted = predictions)
      }

      return(predictions)
    }
  )
)
