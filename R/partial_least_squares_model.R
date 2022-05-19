#' @importFrom R6 R6Class
#' @importFrom pls plsr selectNcomp MSEP RMSEP mvrValstats

#' @include utils.R
#' @include globals.R
#' @include model.R
#' @include model_helpers.R

PartialLeastSquaresModel <- R6Class(
  classname = "PartialLeastSquaresModel",
  inherit = Model,
  public = list(
    # Properties --------------------------------------------------

    optimal_components_num = NULL,

    # Constructor --------------------------------------------------

    initialize = function(..., method, scale) {
      super$initialize(
        ...,
        name = "Partial Least Squares",
        allow_coefficients = TRUE
      )

      self$fit_params$method <- prepare_partial_least_squares_method(method)
      self$fit_params$scale <- scale
    },

    fit = function(...) {
      super$fit(...)

      self$optimal_components_num <- self$fitted_model$components_num
    },
    predict = function(x, components_num) {
      self$fit_params$predict_components_num <- nonull(
        components_num,
        self$optimal_components_num
      )

      super$predict(x)
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_univariate_y = function() {
      super$prepare_univariate_y()

      if(!is_numeric_response(self$responses$y$type)) {
        warning(
          "Partial Least Squares model only accepts numeric responses, so y ",
          "was converted to numeric"
        )

        self$y <- as.numeric(self$y)
        self$responses$y <- list(
          type = RESPONSE_TYPES$CONTINUOUS,
          levels = NULL
        )
      }
    },
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

    train_univariate = function(x, y, fit_params) {
      # In this format for multivariate analysis
      data <- data.frame(y, x, check.names = FALSE)

      tune_model <- plsr(
        formula = fit_params$model_formula,
        data = data,
        scale = fit_params$scale,
        method = fit_params$method,
        validation = "CV",
        segments = 10,
        segment.type = "random"
      )

      model <- plsr(
        formula = fit_params$model_formula,
        data = data,
        scale = fit_params$scale,
        method = fit_params$method,
        validation = "none"
      )

      model$components_num <- max(
        selectNcomp(
          tune_model,
          method = "onesigma",
          plot = FALSE
        ),
        1
      )

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
        ncomp = fit_params$predict_components_num,
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

      tune_model <- plsr(
        formula = fit_params$model_formula,
        data = data,
        scale = fit_params$scale,
        method = fit_params$method,
        validation = "CV",
        segments = 10,
        segment.type = "random"
      )

      model <- plsr(
        formula = fit_params$model_formula,
        data = data,
        scale = fit_params$scale,
        method = fit_params$method,
        validation = "none"
      )

      components_loss <- RMSEP(tune_model)$val[1, , ]
      components_loss <- as.numeric(apply(components_loss, 2, sum))
      # -1 because the intercept column (the first one) is counted
      model$components_num <- max(which.min(components_loss) - 1, 1)

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
        ncomp = fit_params$predict_components_num,
        type = "response"
      )
      # The observations, the response variables and the model sizes.
      all_predictions <- all_predictions[, , 1]

      predictions <- list()

      for (response_name in names(responses)) {
        predictions[[response_name]] <- list(
          predicted = as.numeric(all_predictions[, response_name])
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


#' @title Predict Partial Least Squares model
#'
#' @description
#' Obtains the predictions using a fitted model object of class
#' `PartialLeastSquaresModel`.
#'
#' @inheritParams predict.Model
#' @param components_num (`numeric(1)`) A numeric value with the number of
#'   components to use to compute the predictions. `NULL` by default which uses
#'   the optimal number of components determined with cross validation when the
#'   model was fitted.
#'
#' @return
#' ## When `format` is `"list"`
#'
#' For univariate models a named `list` with the element `"predicted"` which
#' contains the predicted values is returned.
#'
#' For multivariate models a named `list` is returned where there is an named
#' element for each response variable in the fitted model. Each element of this
#' list contains a inner `list` in the same format as described for the
#' univariate case.
#'
#' ## When `format` is `"data.frame"`
#'
#' For univariate models a `data.frame` with the column `predicted` which
#' contains the predicted values.
#'
#' For multivariate models a `data.frame` with a column for each response
#' variable with the predicted values of each response.
#'
#' @examples
#' \dontrun{
#' # Use all default hyperparameters -------------------------------------------
#' x <- to_matrix(iris[, -1])
#' y <- iris$Sepal.Length
#' model <- partial_least_squares(x, y)
#'
#' # Obtain the optimal number of components to use with predict
#' model$optimal_components_num
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values
#' predictions$predicted
#'
#' # Predict with a non optimal number of components ---------------------------
#' x <- to_matrix(iris[, -1])
#' y <- iris$Sepal.Length
#' model <- partial_least_squares(x, y, method = "orthogonal")
#'
#' # Obtain the optimal number of components to use with predict
#' model$optimal_components_num
#'
#' # Predict using the fitted model with the optimal number of components
#' predictions <- predict(model, x)
#' # Obtain the predicted values
#' predictions$predicted
#'
#' # Predict using the fitted model without the optimal number of components
#' predictions <- predict(model, x, components_num = 2)
#' # Obtain the predicted values
#' predictions$predicted
#'
#' # Multivariate analysis -----------------------------------------------------
#' x <- to_matrix(iris[, -c(1, 2)])
#' y <- iris[, c(1, 2)]
#' model <- partial_least_squares(x, y, method = "wide_kernel")
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values of the first response variable
#' predictions$Sepal.Length$predicted
#' # Obtain the predicted values of the second response variable
#' predictions$Sepal.Width$predicted
#'
#' # Obtain the predictions in a data.frame not in a list
#' predictions <- predict(model, x, format = "data.frame")
#' head(predictions)
#' }
#'
#' @export
predict.PartialLeastSquaresModel <- function(model,
                                             x,
                                             components_num = NULL,
                                             format = "list") {
  predictions <- model$predict(x, components_num)

  return(format_predictions(predictions, model$is_multivariate, format))
}
