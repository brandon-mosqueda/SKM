#' @importFrom R6 R6Class

#' @include utils.R

Model <- R6Class(
  classname = "Model",
  public = list(
    # Properties --------------------------------------------------

    name = NULL,
    x = NULL,
    y = NULL,
    responses = list(),
    sparse_kernel = NULL,
    rows_proportion = NULL,
    arc_cosine_deep = NULL,
    validate_params = NULL,
    silently = NULL,

    # Constructor --------------------------------------------------

    initialize = function(x,
                          y,
                          name,
                          is_multivariate,
                          sparse_kernel,
                          rows_proportion,
                          arc_cosine_deep,
                          silently) {
      self$x <- x
      self$y <- y
      self$name <- name
      self$is_multivariate <- is_multivariate
      self$sparse_kernel <- sparse_kernel
      self$rows_proportion <- rows_proportion
      self$arc_cosine_deep <- arc_cosine_deep
      self$silently <- silently
    },

    # Methods --------------------------------------------------

    fit = function() {
      private$prepare_x()
      private$prepare_y()
      private$prepare_others()

      private$train()
    }
  ),
  private = list(
    # Properties --------------------------------------------------

    fitted_model = NULL,

    # Methods --------------------------------------------------

    prepare_x = function(intercept = 0) {
      x <- to_matrix(x, intercept = intercept)
      # remove_no_variance_cols()
      remove_nas()
    },

    prepare_y = function() {
      if (is_multivariate) {
        prepare_univariate_y()
      } else {
        prepare_multivariate_y()
      }
    },
    prepare_others = invisible,
    train = not_implemented_function
  )
)
