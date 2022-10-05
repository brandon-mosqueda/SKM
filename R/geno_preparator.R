#' @importFrom R6 R6Class

#' @include utils.R

GenoPreparator <- R6Class(
  classname = "GenoPreparator",
  public = list(
    # Properties --------------------------------------------------

    Geno = NULL,

    # Methods --------------------------------------------------

    initialize = function(Geno) {
      self$Geno <- Geno
    },

    preprocess = function(sorted_final_lines) {
      self$Geno <- self$Geno[sorted_final_lines, sorted_final_lines]
      self$Geno <- cholesky(self$Geno)
    }
  )
)
