#' @importFrom R6 R6Class
#' @importFrom pls plsr selectNcomp

#' @include utils.R
#' @include model.R
#' @include model_helpers.R

PartialLeastSquaresModel <- R6Class(
  classname = "PartialLeastSquaresModel",
  inherit = Model,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(..., method, components_num, scale) {
      super$initialize(..., name = "Partial Least Squares")

      self$fit_params$method <- prepare_partial_least_squares_method(method)
      self$fit_params$components_num <- components_num
      self$fit_params$scale <- scale
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_multivariate_y = prepare_multivariate_y_only_numeric,
    prepare_others = function() {
      self$fit_params$model_formula <- get_partial_least_squares_formula(
        self$responses,
        self$is_multivariate
      )
    },
    get_x_for_model = function(x, remove_cols = FALSE) {
      return(to_data_frame(x))
    },
    has_to_tune = function() return(FALSE),

    fit = function(...) {
      super$fit(...)

      self$fit_params$components_num <- self$fitted_model$components_num
    },

    train_univariate = function(x, y, fit_params) {
      # In this format for multivariate analysis
      data <- data.frame(y, x, check.names = FALSE)

      model <- plsr(
        formula = fit_params$model_formula,
        data = data,
        scale = fit_params$scale,
        method = fit_params$method,
        validation = "none"
      )

      model$components_num <- fit_params$components_num
      if (is.null(model$components_num)) {
        model$components_num <- selectNcomp(
          model,
          method = "onesigma",
          plot = FALSE
        )
      }

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  fit_params) {
      x <- data.frame(x, check.names = FALSE)
      predictions <- predict(
        model,
        x,
        ncomp = model$components_num,
        type = "response"
      )
      predictions <- list(predicted = c(predictions))

      return(predictions)
    },
    coefficients_univariate = function() {
      coefs <- coef(self$fitted_model)[, , 1]

      return(coefs)
    },

    train_multivariate = function(x, y, fit_params) {
      # In this format for multivariate analysis
      data <- data.frame(y, x, check.names = FALSE)

      model <- plsr(
        formula = fit_params$model_formula,
        data = data,
        scale = fit_params$scale,
        method = fit_params$method,
        validation = "none"
      )

      model$components_num <- model$ncomp

      return(model)
    },
    predict_multivariate = function(model,
                                    x,
                                    responses,
                                    fit_params) {
      x <- data.frame(x, check.names = FALSE)
      all_predictions <- predict(
        model,
        x,
        ncomp = model$components_num,
        type = "response"
      )
      # The observations, the response variables and the model sizes.
      all_predictions <- all_predictions[, , 1]

      predictions <- list()

      for (response_name in names(responses)) {
        predictions[[response]] <- list(
          predicted = all_predictions[, response]
        )
      }

      return(predictions)
    },
    coefficients_multivariate = function() {
      coefs <- list()
      all_coefs <- coef(self$fitted_model)[, , 1]

      for (name in names(self$responses)) {
        coefs[[name]] <- all_coefs[, name]
      }

      return(coefs)
    }
  )
)
