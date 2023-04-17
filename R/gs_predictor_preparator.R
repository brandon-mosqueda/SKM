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
        mutate(
          Line = factor(Line),
          Env = factor(Env),
          OriginalIndex = row_number()
        ) %>%
        arrange(Env, Line)

      lines <- sort(unique(self$Pheno$Line))
      self$Geno <- Geno[lines, lines]

      self$predictors <- tolower(predictors)
    },
    get_new_mapping_indices = function() {
      return(
        self$Pheno %>%
          select(OriginalIndex) %>%
          mutate(NewIndex = row_number()) %>%
          arrange(OriginalIndex) %>%
          pull(NewIndex)
      )
    },

    prepare = not_implemented_function
  )
)
