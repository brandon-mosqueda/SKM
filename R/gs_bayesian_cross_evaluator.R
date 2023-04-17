#' @import dplyr

#' @importFrom R6 R6Class

#' @include utils.R
#' @include bayesian_model.R
#' @include gs_cross_evaluator.R
#' @include gs_bayesian_predictor_preparator.R

GSBayesianCrossEvaluator <- R6Class(
  classname = "GSBayesianCrossEvaluator",
  inherit = GSCrossEvaluator,
  public = list(
    # Properties ---------------------------------------------------------------

    model = NULL,

    iterations_number = NULL,
    burn_in = NULL,
    thinning = NULL,

    initialize = function(Pheno,
                          Geno,
                          traits,
                          model,
                          is_multitrait,
                          predictors,
                          folds,
                          iterations_number,
                          burn_in,
                          thinning) {
      predictor_preparator <- GSBayesianPredictorPreparator$new(
        model = model,
        Pheno = Pheno,
        Geno = Geno,
        predictors = predictors
      )

      super$initialize(
        predictor_preparator = predictor_preparator,
        traits = traits,
        is_multitrait = is_multitrait,
        folds = folds
      )

      self$model <- model
      self$iterations_number <- iterations_number
      self$burn_in <- burn_in
      self$thinning <- thinning
    },

    eval_unitrait_fold = function(trait, fold) {
      ETA <- self$predictor_preparator$X
      y_na <- replace(
        self$predictor_preparator$Pheno[[trait]],
        fold$testing,
        NA
      )

      model <- bayesian_model(
        x = ETA,
        y = y_na,

        iterations_number = self$iterations_number,
        burn_in = self$burn_in,
        thinning = self$thinning,

        verbose = FALSE
      )

      return(model)
    },
    predict_unitrait = function(trait, model, fold) {
      return(predict(
        model,
        fold$testing
      )$predicted)
    },

    eval_multitrait_fold = function(fold) {
      ETA <- self$predictor_preparator$X
      y_na <- self$predictor_preparator$Pheno %>%
        select(all_of(self$traits)) %>%
        mutate_all(~ replace(., fold$testing, NA))

      model <- bayesian_model(
        x = ETA,
        y = y_na,
        iterations_number = self$iterations_number,
        burn_in = self$burn_in,
        thinning = self$thinning,
        verbose = FALSE
      )

      return(model)
    },
    predict_multitrait = function(model, fold) {
      return(predict(
        model,
        fold$testing,
        format = "data.frame"
      ))
    },
    export = function() {
      results <- super$export()
      results$model <- self$model
      results$iterations_number <- self$iterations_number
      results$burn_in <- self$burn_in
      results$thinning <- self$thinning

      class(results) <- "GSBayesian"

      return(results)
    }
  )
)

#' @export
print.GSBayesian <- function(model) {
  print.GSCrossEvaluator(model)
  echo("\n$model: %s", model$model)
  echo("\n$iterations_number: %i", model$iterations_number)
  echo("\n$burn_in: %i", model$burn_in)
  echo("\n$thinning: %i", model$thinning)
}
