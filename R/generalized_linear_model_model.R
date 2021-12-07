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
      super$initialize(
        ...,
        name = "Generalized Linear Model",
        allow_coefficients = TRUE
      )

      self$fit_params$alpha <- alpha
      self$fit_params$lambda <- lambda

      self$fit_params$lambdas_number <- lambdas_number
      self$fit_params$lambda_min_ratio <- lambda_min_ratio
      self$fit_params$records_weights <- records_weights
      self$fit_params$standardize <- standardize
      self$fit_params$fit_intercept <- fit_intercept
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_multivariate_y = prepare_multivariate_y_only_numeric,
    prepare_others = function() {
      self$fit_params$response_family <- get_glmnet_family(
        response_type = self$responses$y$type,
        is_multivariate = self$is_multivariate
      )

      self$fit_params$records_weights <- remove_if_has_more(
        x = self$fit_params$records_weights,
        compare_value = nrow(self$x),
        indices_to_remove = self$removed_rows
      )

      # Evaluate one model first to obtain the lambdas sequence and the perform
      # cross validation by our own
      if (is.null(self$fit_params$lambda)) {
        fit_params <- list(alpha = self$fit_params$alpha[1], lambda = NULL)

        fit_params <- self$fit_params
        fit_params$records_weights <- NULL

        model <- private$train(
          x = self$x,
          y = self$y,
          fit_params = fit_params
        )
        self$fit_params$lambda <- model$lambda
      }
    },

    tune = function() {
      true_other_params <- self$fit_params

      self$fit_params$records_weights <- NULL

      super$tune()

      self$fit_params <- true_other_params
    },

    train_univariate = train_glm,
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  fit_params) {
      if (is_class_response(responses$y$type)) {
        predictions <- predict(model, x, type = "class")
        probabilities <- predict(model, x, type = "response")

        if (is_binary_response(responses$y$type)) {
          # This only returns the probabilities of the second level
          probabilities <- cbind(1 - probabilities, probabilities)
          colnames(probabilities) <- responses$y$levels
        } else {
          probabilities <- probabilities[, , 1]
        }

        predictions <- factor(c(predictions), levels = responses$y$levels)

        return(list(
          predicted = predictions,
          probabilities = as.data.frame(probabilities)
        ))
      } else {
        return(list(predicted = c(predict(model, x))))
      }
    },
    coefficients_univariate = function() {
      if (is_categorical_response(self$responses$y$type)) {
        coefs_cols_num <- ncol(self$x)
        if (self$fit_params$fit_intercept) {
          coefs_cols_num <- coefs_cols_num + 1
        }
        coefs <- matrix(, 0, coefs_cols_num)
        classes <- self$responses$y$levels
        all_coefs <- coef(self$fitted_model)

        for (class in classes) {
          coefs <- rbind(coefs, as.numeric(all_coefs[[class]]))
        }

        colnames(coefs) <- rownames(all_coefs[[1]])
        if (self$fit_params$fit_intercept) {
          colnames(coefs)[1] <- "(Intercept)"
        }
        rownames(coefs) <- classes
      } else {
        temp <- coef(self$fitted_model)
        coefs <- as.numeric(temp)
        names(coefs) <- rownames(temp)
      }

      return(coefs)
    },

    train_multivariate = train_glm,
    predict_multivariate = function(model,
                                    x,
                                    responses,
                                    fit_params) {
      all_predictions <- predict(model, x)
      all_predictions <- all_predictions[, , 1]
      predictions <- list()

      for (response in names(responses)) {
        predictions[[response]] <- list(
          predicted = all_predictions[, response]
        )
      }

      return(predictions)
    },
    coefficients_multivariate = function() {
      coefs <- list()
      all_coefs <- coef(self$fitted_model)

      for (name in names(self$responses)) {
        temp <- as.numeric(all_coefs[[name]])
        names(temp) <- rownames(all_coefs[[name]])

        if (self$fit_params$fit_intercept) {
          names(temp)[1] <- "(Intercept)"
        }

        coefs[[name]] <- temp
      }

      return(coefs)
    }
  )
)
