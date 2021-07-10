#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R

Model <- R6Class(
  classname = "Model",
  public = list(
    # Properties --------------------------------------------------

    fitted_model = NULL,
    name = NULL,
    is_multivariate = NULL,
    responses = list(),

    x = NULL,
    y = NULL,
    kernel = NULL,
    degree = NULL,
    gamma = NULL,
    coef0 = NULL,
    rows_proportion = NULL,
    arc_cosine_deep = NULL,
    validate_params = NULL,
    silently = NULL,

    # Constructor --------------------------------------------------

    initialize = function(x,
                          y,
                          name,
                          is_multivariate,
                          kernel,
                          degree,
                          gamma,
                          coef0,
                          rows_proportion,
                          arc_cosine_deep,
                          silently) {
      self$x <- x
      self$y <- y
      self$name <- name
      self$is_multivariate <- is_multivariate
      self$kernel <- kernel
      self$degree <- degree
      self$gamma <- gamma
      self$coef0 <- coef0
      self$rows_proportion <- rows_proportion
      self$arc_cosine_deep <- arc_cosine_deep
      self$silently <- silently
    },

    # Methods --------------------------------------------------

    fit = function() {
      private$prepare_x()
      private$prepare_y()
      private$prepare_others()

      if (self$silently) {
        hush(private$train(), all = TRUE)
      } else {
        private$train()
      }
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

    prepare_univariate_y = prepare_univariate_y,
    prepare_multivariate_y = prepare_multivariate_y,
    prepare_others = invisible,
    train = not_implemented_function,
    predict = not_implemented_function
  )
)

#' @export
predict.Model <- function(model, x) {
  return(model$predict(x))
}
