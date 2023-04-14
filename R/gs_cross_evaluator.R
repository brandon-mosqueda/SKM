#' @import dplyr

#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include folds_manager.R

GSCrossEvaluator <- R6Class(
  classname = "GSCrossEvaluator",
  public = list(
    # Properties ---------------------------------------------------------------

    is_multivariate = NULL,
    traits = NULL,
    geno_preparator = NULL,
    predictor_preparator = NULL,
    folds_manager = NULL,

    unique_lines = NULL,
    execution_time = NULL,

    initialize = function(geno_preparator,
                          predictor_preparator_class,
                          traits,
                          is_multivariate,

                          folds) {
      self$is_multivariate <- is_multivariate
      self$traits <- traits
      self$unique_lines <- sort(unique(self$Pheno$Line))

      self$geno_preparator <- geno_preparator
      self$geno_preparator$prepare(self$unique_lines)

      self$predictor_preparator <- predictor_preparator_class$new(
        Pheno = self$Pheno,
        geno_preparator = self$geno_preparator,
        predictors = predictors
      )

      self$folds_manager <- FoldsManager$new(folds)
    },

    # Methods ------------------------------------------------------------------

    eval_multitrait = function() {
      Predictions <- list()

      while(self$folds_manager$has_next()) {
        fold <- self$folds_manager$get_next()
        self$folds_manager$print_current(level = 1)

        fold_model <- private$eval_fold(fold)
        predicted <- private$predict_multitrait(fold_model)

        for (trait in self$traits) {
          Predictions <- Predictions %>%
            bind_rows(tibble(
              Trait = trait,
              Fold = self$folds_manager$current_number,
              Line = self$Pheno$Line[fold$testing],
              Env = self$Pheno$Env[fold$testing],
              Observed = self$Pheno[[trait]][fold$testing],
              Predicted = predicted[[trait]]
            ))
        }
      }

      return(Predictions)
    },

    eval_unitrait = function() {
      Predictions <- list()

      for (trait in self$traits) {
        echo("* Trait: %s", trait)

        TraitPredictions <- tibble()

        while(self$folds_manager$has_next()) {
          fold <- self$folds_manager$get_next()
          self$folds_manager$print_current(level = 1)

          fold_model <- private$eval_fold(trait, fold)
          predicted <- private$predict_unitrait(trait, fold_model)

          TraitPredictions <- TraitPredictions %>%
            bind_rows(tibble(
              Trait = trait,
              Fold = self$folds_manager$current_number,
              Line = self$Pheno$Line[fold$testing],
              Env = self$Pheno$Env[fold$testing],
              Observed = self$Pheno[[trait]][fold$testing],
              Predicted = predicted
            ))
        }

        Predictions[[trait]] <- TraitPredictions
      }

      return(Predictions)
    },

    eval = function() {
      if (self$is_multivariate) {
        return(self$eval_multitrait())
      }

      return(self$eval_unitrait())
    }

  ),
  private = list(
    # Methods ------------------------------------------------------------------

    eval_fold = not_implemented_function,
    predict_unitrait = not_implemented_function,
    predict_multitrait = not_implemented_function
  )
)
