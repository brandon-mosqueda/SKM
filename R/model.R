#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R

Model <- R6Class(
  classname = "Model",
  public = list(
    # Properties --------------------------------------------------

    fitted_model = NULL,
    best_hyperparams = NULL,
    tunable_hyperparams = NULL,
    name = NULL,
    is_multivariate = NULL,
    responses = list(),

    tune_cv_type = NULL,
    tune_folds_number = NULL,
    tune_testing_proportion = NULL,

    x = NULL,
    y = NULL,
    kernel = NULL,
    degree = NULL,
    gamma = NULL,
    coef0 = NULL,
    rows_proportion = NULL,
    arc_cosine_deep = NULL,
    validate_params = NULL,
    verbose = NULL,

    # Constructor --------------------------------------------------

    initialize = function(x,
                          y,
                          name,
                          tunable_hyperparams,
                          tune_cv_type,
                          tune_folds_number,
                          tune_testing_proportion,
                          is_multivariate,
                          kernel,
                          degree,
                          gamma,
                          coef0,
                          rows_proportion,
                          arc_cosine_deep,
                          verbose) {
      self$x <- x
      self$y <- y
      self$name <- name
      self$tunable_hyperparams <- tunable_hyperparams
      self$tune_cv_type <- tune_cv_type
      self$tune_folds_number <- tune_folds_number
      self$tune_testing_proportion <- tune_testing_proportion
      self$is_multivariate <- is_multivariate
      self$kernel <- kernel
      self$degree <- degree
      self$gamma <- gamma
      self$coef0 <- coef0
      self$rows_proportion <- rows_proportion
      self$arc_cosine_deep <- arc_cosine_deep
      self$verbose <- verbose
    },

    # Methods --------------------------------------------------

    fit = function() {
      private$prepare_x()
      private$prepare_y()
      private$prepare_others()

      wrapper_function <- if (self$verbose) hush else invisible

      wrapper_function(private$tune())

      wrapper_function(private$set_fitted_model())
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_x = function() {
      self$x <- prepare_x(
        x = self$x,
        kernel = self$kernel,
        rows_proportion = self$rows_proportion,
        arc_cosine_deep = self$arc_cosine_deep,
        degree = self$degree,
        gamma = self$gamma,
        coef0 = self$coef0
      )
    },
    prepare_y = function() {
      if (self$is_multivariate) {
        private$prepare_multivariate_y()
      } else {
        private$prepare_univariate_y()
      }
    },
    prepare_others = function() {
      self$degree <- prepare_degree(self$kernel, self$degree)
      self$gamma <- prepare_gamma(self$kernel, self$gamma)
      self$coef0 <- prepare_coef0(self$kernel, self$coef0)
    },
    has_to_tune = function() {
      for (hyperparam in self$tunable_hyperparams) {
        values <- nonull(self[[hyperparam]], private[[hyperparam]])

        if (length(values) > 1) {
          return(TRUE)
        }
      }

      return(FALSE)
    },
    tune = function() {
      flags <- list()
      for (hyperparam in self$tunable_hyperparams) {
        values <- nonull(self[[hyperparam]], private[[hyperparam]])

        flags[[hyperparam]] <- values
      }

      if (private$has_to_tune()) {
        cross_validator <- get_cross_validator(
          type = self$tune_cv_type,
          records_number = nrow(self$x),
          folds_number = self$folds_number,
          testing_proportion = self$testing_proportion
        )

        flags_grid <- expand.grid(flags)
        for (combination in flags_grid) {

        }
      }
    },
    set_fitted_model = function() {
      self$fitted_model <- private$train(self)
    },

    prepare_univariate_y = prepare_univariate_y,
    prepare_multivariate_y = prepare_multivariate_y,
    train = not_implemented_function,
    predict = not_implemented_function
  )
)

#' @export
predict.Model <- function(model, x) {
  return(model$predict(x))
}
