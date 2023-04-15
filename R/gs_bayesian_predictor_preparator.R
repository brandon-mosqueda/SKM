#' @importFrom R6 R6Class

#' @include utils.R
#' @include gs_predictor_preparator.R

GSBayesianPredictorPreparator <- R6Class(
  classname = "GSBayesianPredictorPreparator",
  inherit = GSPredictorPreparator,
  public = list(
    # Properties ---------------------------------------------------------------

    model = NULL,

    # Methods ------------------------------------------------------------------

    initialize = function(model, ...) {
      super$initialize(...)

      self$model <- tolower(model)

      if (self$model != "gblup") {
        self$Geno <- cholesky(self$Geno)
      }
    },

    prepare = function() {
      if (self$model == "gblup") {
        private$prepare_gblup()
      } else {
        private$prepare_bayes()
      }
    }
  ),
  private = list(
    # Methods ------------------------------------------------------------------

    prepare_gblup = function() {
      self$X <- list()

      Line <- dummy_matrix(self$Pheno$Line)
      Env <- dummy_matrix(self$Pheno$Env)
      Env <- Env %*% t(Env) / ncol(Env)

      if ("line" %in% self$predictors || "envxline" %in% self$predictors) {
        GenoLine <- Line %*% self$Geno %*% t(Line)

        if ("line" %in% self$predictors) {
          self$X$Line <- list(x = GenoLine, model = self$model)
        }

        if ("envxline" %in% self$predictors) {
          self$X$EnvxLine <- list(x = Env * GenoLine, model = self$model)
        }
      }

      if ("env" %in% self$predictors) {
        self$X$Env <- list(x = Env, model = self$model)
      }

      return(self$X)
    },

    prepare_bayes = function() {
      self$X <- list()

      Line <- dummy_matrix(self$Pheno$Line)
      Env <- dummy_matrix(self$Pheno$Env)

      if ("line" %in% self$predictors || "envxline" %in% self$predictors) {
        GenoLine <- Line %*% self$Geno

        if ("line" %in% self$predictors) {
          self$X$Line <- list(x = GenoLine, model = self$model)
        }

        if ("envxline" %in% self$predictors) {
          self$X$EnvxLine <- list(
            x = model.matrix(~ 0 + GenoLine:Env),
            model = self$model
          )
        }
      }

      if ("env" %in% self$predictors) {
        self$X$Env <- list(x = Env, model = self$model)
      }

      return(self$X)
    }
  )
)
