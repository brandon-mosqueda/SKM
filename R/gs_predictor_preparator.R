#' @import dplyr

#' @importFrom R6 R6Class

GSPredictorPreparator <- R6Class(
  classname = "GSPredictorPreparator",
  public = list(
    # Properties ---------------------------------------------------------------

    Pheno = NULL,
    geno_preparator = NULL,
    predictors = NULL,
    X = NULL,

    # Methods ------------------------------------------------------------------

    initialize = function(Pheno,
                          geno_preparator,
                          predictors) {
      self$Pheno <- self$prepare_pheno(Pheno)
      self$geno_preparator <- geno_preparator
      self$predictors <- tolower(predictors)
    },

    prepare_pheno = function(Pheno) {
      Pheno <- Pheno %>%
        as_tibble() %>%
        mutate(Line = factor(Line), Env = factor(Env)) %>%
        arrange(Env, Line)

      return(Pheno)
    },

    prepare = not_implemented_function
  )
)
