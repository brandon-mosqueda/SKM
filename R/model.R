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
                          tune_type = NULL,
                          tune_cv_type = NULL,
                          tune_folds_number = NULL,
                          tune_testing_proportion = NULL,
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
        x <- x[, -self$removed_x_cols]
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
          grid_proportion = self$tune_grid_proportion,

          tune_bayes_iterations_number = self$tune_bayes_iterations_number,
          tune_bayes_samples_number = self$tune_bayes_samples_number
        )

        tuner$tune()
        self$best_hyperparams <- tuner$best_combination
        self$hyperparams_grid <- tuner$all_combinations
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

#' @export
predict.Model <- function(model, x) {
  return(model$predict(x))
}

#' @export
coef.Model <- function(model) {
  return(model$coefficients())
}
