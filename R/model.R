#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R

Model <- R6Class(
  classname = "Model",
  public = list(
    # Properties --------------------------------------------------

    name = NULL,
    is_multivariate = NULL,
    responses = list(),

    fitted_model = NULL,
    best_hyperparams = NULL,
    hyperparams_grid = NULL,
    hyperparams = list(),
    other_params = list(),

    tune_cv_type = NULL,
    tune_folds_number = NULL,
    tune_testing_proportion = NULL,

    x = NULL,
    y = NULL,
    validate_params = NULL,
    execution_time = NULL,

    # Constructor --------------------------------------------------

    initialize = function(x,
                          y,
                          name,
                          tune_cv_type,
                          tune_folds_number,
                          tune_testing_proportion,
                          is_multivariate,
                          kernel,
                          degree,
                          gamma,
                          coef0,
                          rows_proportion,
                          arc_cosine_deep) {
      self$x <- x
      self$y <- y
      self$name <- name
      self$tune_cv_type <- tune_cv_type
      self$tune_folds_number <- tune_folds_number
      self$tune_testing_proportion <- tune_testing_proportion
      self$is_multivariate <- is_multivariate

      self$other_params <- list()
      self$other_params$kernel <- kernel
      self$other_params$rows_proportion <- rows_proportion
      self$other_params$arc_cosine_deep <- arc_cosine_deep

      self$hyperparams <- list()
      # Set to NULL those parameters that there is no need to tune
      self$hyperparams$degree <- prepare_degree(kernel, degree)
      self$hyperparams$gamma <- prepare_gamma(kernel, gamma)
      self$hyperparams$coef0 <- prepare_coef0(kernel, coef0)
    },

    # Methods --------------------------------------------------

    fit = function() {
      private$prepare_x()
      private$prepare_y()
      private$prepare_others()

      private$tune()

      echo("*** Fitting %s model ***", self$name)
      self$fitted_model <- private$train(
        x = self$x,
        y = self$y,
        hyperparams = self$best_hyperparams,
        other_params = self$other_params
      )
    },
    predict = function(...,
                       x,
                       hyperparams,
                       other_params) {
      x <- private$get_x_for_model(
        x = x,
        hyperparams = hyperparams,
        other_params = other_params
      )

      if (self$is_multivariate) {
        private$predict_multivariate(
          ...,
          x = x,
          hyperparams = hyperparams,
          other_params = other_params
        )
      } else {
        private$predict_univariate(
          ...,
          x = x,
          hyperparams = hyperparams,
          other_params = other_params
        )
      }
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_x = function() {
      self$x <- private$get_x_for_model(
        x = self$x,
        hyperparams = self$hyperparams,
        other_params = self$other_params
      )
    },
    get_x_for_model = function(x, hyperparams, other_params) {
      return(prepare_x(
        x = x,
        kernel = other_params$kernel,
        rows_proportion = other_params$rows_proportion,
        arc_cosine_deep = other_params$arc_cosine_deep,
        degree = hyperparams$degree,
        gamma = hyperparams$gamma,
        coef0 = hyperparams$coef0
      ))
    },
    prepare_y = function() {
      if (self$is_multivariate) {
        private$prepare_multivariate_y()
      } else {
        private$prepare_univariate_y()
      }
    },
    has_to_tune = function() {
      for (hyperparam in self$hyperparams) {
        if (length(hyperparam) > 1) {
          return(TRUE)
        }
      }

      return(FALSE)
    },
    tune = function() {
      if (private$has_to_tune()) {
        training_function <- private$train_univariate
        predict_function <- private$predict_univariate
        if (self$is_multivariate) {
          training_function <- private$train_multivariate
          predict_function <- private$predict_multitrain_multivariate
        }

        tuner <- Tuner$new(
          x = self$x,
          y = self$y,
          responses = self$responses,
          is_multivariate = self$is_multivariate,
          training_function = training_function,
          predict_function = predict_function,
          hyperparams = self$hyperparams,
          other_params = self$other_params,
          cv_type = self$tune_cv_type,
          folds_number = self$tune_folds_number,
          testing_proportion = self$tune_testing_proportion
        )

        tuner$tune()
        self$best_hyperparams <- tuner$best_combination
        self$hyperparams_grid <- tuner$all_combinations
      } else {
        self$best_hyperparams <- self$hyperparams
        self$hyperparams_grid <- self$hyperparams
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
    predict_multivariate = not_implemented_function
  )
)

#' @export
predict.Model <- function(model, x) {
  return(model$predict(
    model = model$fitted_model,
    x = x,
    responses = model$responses,
    is_multivariate = model$is_multivariate,
    other_params = model$other_params,
    hyperparams = model$best_hyperparams
  ))
}
