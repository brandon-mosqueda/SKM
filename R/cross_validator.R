#' @importFrom R6 R6Class

#' @include utils.R

CrossValidator <- R6Class(
  classname = "CrossValidator",
  public = list(
    # Properties --------------------------------------------------

    name = NULL,
    folds_number = NULL,
    records_number = NULL,
    testing_proportion = NULL,

    # Constructor --------------------------------------------------

    initialize = function(name,
                          folds_number,
                          records_number,
                          testing_proportion = NULL) {
      self$name <- name
      self$folds_number <- folds_number
      self$records_number <- records_number
      self$testing_proportion <- testing_proportion
    },

    # Methods --------------------------------------------------

    get_folds = not_implemented_function,
    print_advance = function(current_fold, tabs_number = 1) {
      tabs <- get_tabs(tabs_number)

      echo("%s%s: %s/%s", tabs, self$name, current_fold, self$folds_number)
    }
  )
)
