#' @importFrom R6 R6Class

MarkersPreparator <- R6Class(
  classname = "MarkersPreparator",
  public = list(
    # Properties --------------------------------------------------

    Geno = NULL,

    # Methods --------------------------------------------------

    initialize = function(Geno) {
      self$Geno <- Geno
    },

    preprocess = function(sorted_final_lines) {
      self$Geno <- remove_no_variance_cols(self$Geno)
      self$Geno <- self$Geno[sorted_final_lines, , drop = FALSE]
      self$Geno <- scale(self$Geno) / sqrt(ncol(self$Geno))
    }
  )
)
