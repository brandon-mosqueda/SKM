#' @importFrom R6 R6Class
#' @importFrom glmnet glmnet

#' @include utils.R
#' @include model.R
#' @include model_helpers.R

GeneralizedLinearModel <- R6Class(
  classname = "GeneralizedLinearModel",
  inherit = Model,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(...,
                          alpha,
                          lambda,

                          lambdas_number,
                          lambda_min_ratio,
                          records_weights,
                          standardize,
                          fit_intercept) {
      super$initialize(..., name = "Generalized Linear Model")

      self$hyperparams$alpha <- alpha
      self$hyperparams$lambda <- lambda

      self$other_params$lambdas_number <- lambdas_number
      self$other_params$lambda_min_ratio <- lambda_min_ratio
      self$other_params$records_weights <- records_weights
      self$other_params$standardize <- standardize
      self$other_params$fit_intercept <- fit_intercept
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_others = function() {
      self$other_params$response_family <- get_glmnet_family(
        self$responses$y$type,
        self$is_multivariate
      )

      # glm only accepts multivariate for numeric variables
      if (self$is_multivariate) {
        are_all_numeric <- all(sapply(
          self$responses,
          function(response) is_numeric_response(response$type)
        ))

        if (!are_all_numeric) {
          warning(
            "In generalized linear multivariate models it can only be used ",
            "numeric responses variables, so some of the responses were ",
            "converted to numeric"
          )
          self$y <- data.matrix(self$y)
        }
      }

      # Evaluate one model first to obtain the lambdas sequence and the perform
      # cross validation by our own
      if (is.null(self$hyperparams$lambda)) {
        hyperparams <- list(alpha = self$hyperparams$alpha[1], lambda = NULL)

        other_params <- self$other_params
        other_params$records_weights <- NULL

        model <- private$train(
          x = self$x,
          y = self$y,
          hyperparams = hyperparams,
          other_params = other_params
        )
        self$hyperparams$lambda <- model$lambda
      }
    },

    tune = function() {
      true_other_params <- self$other_params

      self$other_params$records_weights <- NULL

      super$tune()

      self$other_params <- true_other_params
    },

    train_univariate = train_glm,
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  other_params,
                                  hyperparams) {
      if (is_class_response(responses$y$type)) {
        predictions <- predict(model, x, type = "class")
        probabilities <- predict(model, x, type = "response")

        if (is_binary_response(responses$y$type)) {
          # This only returns the probabilities of the first level
          probabilities <- cbind(1 - probabilities, probabilities)
          colnames(probabilities) <- responses$y$levels
        } else {
          probabilities <- probabilities[, , 1]
        }

        predictions <- factor(c(predictions), levels = responses$y$levels)

        return(list(
          predicted = predictions,
          probabilities = probabilities
        ))
      } else {
        return(list(predicted = c(predict(model, x))))
      }
    },

    train_multivariate = train_glm,
    predict_multivariate = function(model,
                                    x,
                                    responses,
                                    other_params,
                                    hyperparams) {
      all_predictions <- predict(model, x)
      all_predictions <- all_predictions[, , 1]
      predictions <- list()

      for (response in names(self$responses)) {
        predictions[[response]] <- list(
          predicted = all_predictions[, response]
        )
      }

      return(predictions)
    }
  )
)
