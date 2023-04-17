#' @importFrom R6 R6Class

#' @include utils.R

FoldsManager <- R6Class(
  classname = "FoldsManager",
  public = list(
    # Properties ---------------------------------------------------------------

    folds = NULL,
    current_number = 0,

    initialize = function(folds) {
      self$folds <- folds
    },

    # Methods ------------------------------------------------------------------

    has_next = function() {
      return(self$current_number < length(self$folds))
    },

    get_next = function() {
      self$current_number <- self$current_number + 1

      return(self$folds[[self$current_number]])
    },

    print_current = function(level = 0) {
      echo(
        "%s- Fold %s/%s",
        get_tabs(level),
        self$current_number,
        length(self$folds)
      )
    },
    reset = function() {
      self$current_number <- 0
    },
    remap_indices = function(new_indices) {
      self$folds <- lapply(
        self$folds,
        function(fold) {
          fold$training <- new_indices[fold$training]
          fold$testing <- new_indices[fold$testing]

          return(fold)
        }
      )
    }
  )
)
