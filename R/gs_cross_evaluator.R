#' @import dplyr

#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include folds_manager.R

GSCrossEvaluator <- R6Class(
  classname = "GSCrossEvaluator",
  public = list(
    # Properties ---------------------------------------------------------------

    predictor_preparator = NULL,

    is_multitrait = NULL,
    traits = NULL,
    folds_manager = NULL,

    Predictions = NULL,
    execution_time = NULL,

    initialize = function(predictor_preparator,
                          traits,
                          is_multitrait,
                          folds) {
      self$is_multitrait <- is_multitrait
      self$traits <- traits
      self$predictor_preparator <- predictor_preparator
      self$predictor_preparator$prepare()

      self$folds_manager <- FoldsManager$new(folds)
      self$folds_manager$remap_indices(
        self$predictor_preparator$get_new_mapping_indices()
      )
    },

    # Methods ------------------------------------------------------------------

    eval_multitrait = function() {
      Predictions <- list()
      Pheno <- self$predictor_preparator$Pheno

      while(self$folds_manager$has_next()) {
        fold <- self$folds_manager$get_next()
        self$folds_manager$print_current(level = 0)

        fold_model <- self$eval_multitrait_fold(fold)
        predicted <- self$predict_multitrait(fold_model, fold)

        for (trait in self$traits) {
          Predictions <- Predictions %>%
            bind_rows(tibble(
              Trait = trait,
              Fold = self$folds_manager$current_number,
              Line = Pheno$Line[fold$testing],
              Env = Pheno$Env[fold$testing],
              Observed = Pheno[[trait]][fold$testing],
              Predicted = predicted[[trait]]
            ))
        }
      }

      return(Predictions)
    },
    eval_unitrait = function() {
      Predictions <- list()
      Pheno <- self$predictor_preparator$Pheno

      i <- 1
      for (trait in self$traits) {
        echo("* Trait '%s' (%s/%s)", trait, i, length(self$traits))
        i <- i + 1

        while(self$folds_manager$has_next()) {
          fold <- self$folds_manager$get_next()
          self$folds_manager$print_current(level = 1)

          fold_model <- self$eval_unitrait_fold(trait, fold)
          predicted <- self$predict_unitrait(trait, fold_model, fold)

          Predictions <- Predictions %>%
            bind_rows(tibble(
              Trait = trait,
              Fold = self$folds_manager$current_number,
              Line = Pheno$Line[fold$testing],
              Env = Pheno$Env[fold$testing],
              Observed = Pheno[[trait]][fold$testing],
              Predicted = predicted
            ))
        }

        self$folds_manager$reset()
      }

      return(Predictions)
    },
    eval = function() {
      if (self$is_multitrait) {
        self$Predictions <- self$eval_multitrait()
      } else {
        self$Predictions <- self$eval_unitrait()
      }

      self$Predictions <- self$Predictions %>%
        mutate(
          Fold = factor(Fold),
          Trait = factor(Trait)
        )
    },
    export = function(path) {
      results <- list(
        Pheno = self$predictor_preparator$Pheno,
        Geno = self$predictor_preparator$Geno,
        traits = self$traits,
        is_multitrait = self$is_multitrait,
        Predictions = self$Predictions,
        execution_time = self$execution_time,
        folds = self$folds_manager$folds
      )

      class(results) <- "GSCrossEvaluator"

      return(results)
    },
    eval_unitrait_fold = not_implemented_function,
    eval_multitrait_fold = not_implemented_function,
    predict_unitrait = not_implemented_function,
    predict_multitrait = not_implemented_function
  )
)

print.GSCrossEvaluator <- function(model) {
  echo("$Predictions")
  print(model$Predictions, n = 5)
  echo("\n$traits: %s", paste0(model$traits, collapse = ", "))
  echo("\n$is_multitrait: %s", model$is_multitrait)
  echo("\n$folds: %s", length(model$folds))
  echo(
    "\n$execution_time: %f mins",
    as.numeric(model$execution_time, units = "mins")
  )
  echo(
    "\n$Pheno\n\tenvs_num: %i\n\tlines_num: %i\n\trows_num: %i",
    lunique(model$Pheno$Env),
    lunique(model$Pheno$Line),
    nrow(model$Pheno)
  )
  echo(
    "\n$Geno\n\trows_num: %i\n\tcols_num: %i",
    nrow(model$Geno),
    ncol(model$Geno)
  )
}
