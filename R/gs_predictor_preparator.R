#' @import dplyr

#' @importFrom R6 R6Class

GSPredictorPreparator <- R6Class(
  classname = "GSPredictorPreparator",
  public = list(
    # Properties ---------------------------------------------------------------

    Pheno = NULL,
    Geno = NULL,
    predictors = NULL,
    X = NULL,

    # Methods ------------------------------------------------------------------

    initialize = function(Pheno,
                          Geno,
                          predictors) {
      self$Pheno <- Pheno %>%
        as_tibble() %>%
        mutate(Line = factor(Line), Env = factor(Env)) %>%
        arrange(Env, Line)

      lines <- sort(unique(self$Pheno$Line))
      self$Geno <- Geno[lines, lines]

      self$predictors <- tolower(predictors)
    },

    prepare = not_implemented_function
  )
)
