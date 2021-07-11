#' @importFrom R6 R6Class

#' @include cross_validator.R

CustomCV <- R6Class(
  classname = "CustomCV",
  inherit = CrossValidator,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(..., folds, name = "CustomCV") {
      private$folds <- folds
      folds_number <- length(folds)

      super$initialize(..., folds_number = folds_number, name = name)
    },

    # Methods --------------------------------------------------

    get_folds = function() {
      return(private$folds)
    }
  ),
  private = list(
    # Properties --------------------------------------------------

    folds = NULL
  )
)
