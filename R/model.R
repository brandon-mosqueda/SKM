#' @importFrom R6 R6Class

#' @include utils.R
#' @include tuner.R
#' @include model_helpers.R
#' @include validator.R

Model <- R6Class(
  classname = "Model",
  public = list(
    # Properties --------------------------------------------------

    name = NULL,
    is_multivariate = NULL,
    allow_coefficients = NULL,
    is_x_matrix = NULL,
    responses = list(),

    fitted_model = NULL,
    tuner_class = NULL,
    best_hyperparams = NULL,
    hyperparams_grid = NULL,
    fit_params = list(),

    tune_cv_type = NULL,
    tune_folds_number = NULL,
    tune_testing_proportion = NULL,
    tune_folds = NULL,
    tune_loss_function = NULL,
    tune_grid_proportion = NULL,
    tune_bayes_samples_number = NULL,
    tune_bayes_iterations_number = NULL,

    x = NULL,
    y = NULL,
    removed_x_cols = NULL,
    removed_rows = NULL,
    execution_time = NULL,

    # Constructor --------------------------------------------------

    initialize = function(x,
                          y,
                          name,
                          tune_type = "grid_search",
                          tune_cv_type = NULL,
                          tune_folds_number = NULL,
                          tune_testing_proportion = NULL,
                          tune_folds = NULL,
                          tune_loss_function = NULL,
                          tune_grid_proportion = NULL,
                          tune_bayes_samples_number = NULL,
                          tune_bayes_iterations_number = NULL,
                          is_multivariate = FALSE,
                          allow_coefficients = FALSE,
                          is_x_matrix = TRUE) {
      self$x <- x
      self$y <- y
      self$name <- name
      self$tune_cv_type <- tune_cv_type
      self$tune_folds_number <- tune_folds_number
      self$tune_testing_proportion <- tune_testing_proportion
      self$tune_folds <- tune_folds
      self$tune_loss_function <- tolower(tune_loss_function)
      if (is_empty(self$tune_loss_function)) {
        self$tune_loss_function <- NULL
      }
      self$tune_grid_proportion <- tune_grid_proportion
      self$tune_bayes_samples_number <- tune_bayes_samples_number
      self$tune_bayes_iterations_number <- tune_bayes_iterations_number
      self$is_multivariate <- is_multivariate
      self$allow_coefficients <- allow_coefficients
      self$is_x_matrix <- is_x_matrix

      self$fit_params <- list()

      self$tuner_class <- get_tuner(tune_type)

      if (self$is_multivariate) {
        self$name <- paste0("Multivariate ", self$name)
      }
    },

    # Methods --------------------------------------------------

    fit = function() {
      if (!is.null(self$fitted_model)) {
        stop("The model is already fitted")
      }
      private$prepare_x()
      private$prepare_y()
      private$handle_nas()
      private$prepare_others()

      private$tune()

      self$fit_params <- replace_at_list(self$fit_params, self$best_hyperparams)

      echo("*** Fitting %s model ***", self$name)
      self$fitted_model <- private$train(
        x = self$x,
        y = self$y,
        fit_params = self$fit_params
      )
    },
    predict = function(x) {
      if (!is.null(x)) {
        assert_x(x, expected_matrix = self$is_x_matrix)
      }

      x <- private$get_x_for_model(x, remove_cols = FALSE)
      if (!is.null(self$removed_x_cols)) {
        x <- x[, -self$removed_x_cols, drop = FALSE]
      }

      predict_function <- ifelse(
        self$is_multivariate,
        private$predict_multivariate,
        private$predict_univariate
      )

      predict_function(
        model = self$fitted_model,
        x = x,
        responses = self$responses,
        fit_params = self$fit_params
      )
    },
    coefficients = function() {
      if (self$allow_coefficients) {
        if (self$is_multivariate) {
          private$coefficients_multivariate()
        } else {
          private$coefficients_univariate()
        }
      } else {
        warning(self$name, " does not computes any type of coefficients.")
      }
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_x = function() {
      self$x <- private$get_x_for_model(self$x)
      self$removed_x_cols <- attr(self$x, "removed_cols")

      if (!is.null(self$removed_x_cols)) {
        warning(
          length(self$removed_x_cols),
          " columns were removed from x because they has no variance ",
          "See $removed_x_cols field to see what columns were removed."
        )
      }
    },
    get_x_for_model = function(x, remove_cols = TRUE) {
      colnames(x) <- get_cols_names(x)

      if (remove_cols) {
        x <- remove_no_variance_cols(x)
      }

      return(x)
    },
    prepare_y = function() {
      if (self$is_multivariate) {
        private$prepare_multivariate_y()
      } else {
        private$prepare_univariate_y()
      }
    },
    handle_nas = function() {
      nas_x_rows <- which_is_na(self$x)
      nas_y_rows <- which_is_na(self$y)

      if (!is.null(nas_x_rows) || !is.null(nas_y_rows)) {
        self$removed_rows <-  union(nas_x_rows, nas_y_rows)

        self$x <- get_records(self$x, -self$removed_rows)
        self$y <- get_records(self$y, -self$removed_rows)

        warning(
          length(self$removed_rows),
          " rows were removed because it has NA values in x and/or y. ",
          "See $removed_rows field to see what rows were removed."
        )
      }
    },
    has_to_tune = function() {
      for (hyperparam in self$fit_params) {
        if (is_hyperparam(hyperparam)) {
          return(TRUE)
        }
      }

      return(FALSE)
    },
    get_hyperparams = function() {
      hyperparams <- list()

      for (param_name in names(self$fit_params)) {
        param <- self$fit_params[[param_name]]

        if (is_hyperparam(param)) {
          hyperparams[[param_name]] <- param
        }
      }

      return(hyperparams)
    },
    tune = function() {
      if (private$has_to_tune()) {
        training_function <- private$train_univariate
        predict_function <- private$predict_univariate
        if (self$is_multivariate) {
          training_function <- private$train_multivariate
          predict_function <- private$predict_multivariate
        }

        hyperparams <- private$get_hyperparams()

        tuner <- self$tuner_class$new(
          x = self$x,
          y = self$y,
          responses = self$responses,
          is_multivariate = self$is_multivariate,
          training_function = training_function,
          predict_function = predict_function,
          hyperparams = hyperparams,
          fit_params = self$fit_params,
          cv_type = self$tune_cv_type,
          folds_number = self$tune_folds_number,
          testing_proportion = self$tune_testing_proportion,
          folds = self$tune_folds,
          loss_function = self$tune_loss_function,
          grid_proportion = self$tune_grid_proportion,

          iterations_number = self$tune_bayes_iterations_number,
          samples_number = self$tune_bayes_samples_number
        )

        tuner$tune()
        self$best_hyperparams <- tuner$best_combination
        self$hyperparams_grid <- tuner$all_combinations
        self$tune_folds <- tuner$cross_validator$folds
      } else {
        self$hyperparams_grid <- data.frame()
        self$best_hyperparams <- list()
      }
    },
    train = function(...) {
      if (self$is_multivariate) {
        private$train_multivariate(...)
      } else {
        private$train_univariate(...)
      }
    },

    prepare_univariate_y = prepare_univariate_y,
    prepare_multivariate_y = prepare_multivariate_y,

    prepare_others = invisible,

    train_univariate = not_implemented_function,
    train_multivariate = not_implemented_function,

    predict_univariate = not_implemented_function,
    predict_multivariate = not_implemented_function,

    coefficients_univariate = not_implemented_function,
    coefficients_multivariate = not_implemented_function
  )
)

#' @title Predict model
#'
#' @description
#' Obtains the predictions using a fitted model object.
#'
#' @param model (`Model`) An object of a fitted model.
#' @param x (`matrix`) The predictor variables to be used to compute the
#'   predictions. It has to be in the same format provided when fitting the
#'   model.
#' @param format (`character(1)`) The expected format of the predictions. The
#'   available options are `"list"` and `"data.frame"`. `"data.frame"` is more
#'   useful with multivariate models if you want in a tabular structure the
#'   predicted values. See Value section below for more information. `"list"`
#'   by default.
#'
#' @return
#' ## When `format` is `"list"`
#'
#' For univariate models a named `list` with the element `"predicted"` which
#' contains the predicted values is returned. For categorical variables the
#' returned `list` includes the element `"probabilities"` too with a
#' `data.frame` of the predicted probabilities of each class.
#'
#' For multivariate models a named `list` is returned where there is an named
#' element for each response variable in the fitted model. Each element of this
#' list contains a inner `list` in the same format as described for the
#' univariate case, so for categorical variables, a `data.frame` with the
#' predicted probabilities is included too.
#'
#' ## When `format` is `"data.frame"`
#'
#' For univariate models a `data.frame` with the column `predicted` which
#' contains the predicted values. For categorical variables, a column for each
#' class with the predicted probability of this class is included too.
#'
#' For multivariate models a `data.frame` with a column for each response
#' variable with the predicted values of each response.
#'
#' @examples
#' \dontrun{
#' # Univariate analysis -------------------------------------------------------
#' x <- to_matrix(iris[, -5])
#' y <- iris$Species
#' model <- random_forest(x, y)
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values
#' predictions$predicted
#' # Obtain the predicted probabilities
#' predictions$probabilities
#'
#' # Predict using the fitted model with data.frame format
#' predictions <- predict(model, x, format = "data.frame")
#' head(predictions)
#'
#' # Multivariate analysis -----------------------------------------------------
#' x <- to_matrix(iris[, -c(1, 2)])
#' y <- iris[, c(1, 2)]
#' model <- generalized_linear_model(x, y)
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
predict.Model <- function(model, x, format = "list") {
  predictions <- model$predict(x)

  return(format_predictions(predictions, model$is_multivariate, format))
}

#' @export
coef.Model <- function(model) {
  return(model$coefficients())
}
