#' @import dplyr

#' @importFrom R6 R6Class

#' @include utils.R
#' @include bayesian_model.R
#' @include gs_cross_evaluator.R
#' @include gs_bayesian_predictor_preparator.R

GSFastBayesianCrossEvaluator <- R6Class(
  classname = "GSFastBayesianCrossEvaluator",
  inherit = GSCrossEvaluator,
  public = list(
    # Properties ---------------------------------------------------------------

    model = NULL,
    model_name = NULL,
    Variance = NULL,
    prev_trait = NULL,

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

      self$model_name <- model
      self$iterations_number <- iterations_number
      self$burn_in <- burn_in
      self$thinning <- thinning
    },

    eval_unitrait_fold = function(trait, fold) {
      if (is.null(self$model) || self$prev_trait != trait) {
        # This is evaluated always for the first fold
        self$prev_trait <- trait
        ETA <- self$predictor_preparator$X

        y <- self$predictor_preparator$Pheno[[trait]]

        self$model <- bayesian_model(
          x = ETA,
          y = y,

          iterations_number = self$iterations_number,
          burn_in = self$burn_in,
          thinning = self$thinning,

          verbose = FALSE
        )

        self$Variance <- self$get_unitrait_variance(self$model)
      }

      return(self$model)
    },
    get_unitrait_variance = function(model) {
      Pheno <- self$predictor_preparator$Pheno
      ETA <- self$predictor_preparator$X

      # Variance components
      rows_num <- nrow(Pheno)

      EnvVar <- model$fitted_model$ETA$Env$varU
      LineVar <- model$fitted_model$ETA$Line$varU
      ErrorVar <- model$fitted_model$varE

      ## V: Covariance matrix of the trait y
      EnvMatrix <- ETA$Env$x
      LineMatrix <- ETA$Line$x
      envs_num <- length(unique(Pheno$Env))
      V <- (1 * EnvMatrix * EnvVar) +
        (LineMatrix * LineVar + diag(ErrorVar, rows_num))

      if ("envxline" %in% self$predictor_preparator$predictors) {
        LineEnvMatrix <- ETA$EnvxLine$x
        LineEnvVar <- model$fitted_model$ETA$EnvxLine$varU
        V <- V + 1 * LineEnvMatrix * LineEnvVar
      }

      return(solve(V))
    },
    predict_unitrait = function(trait, model, fold) {
      y <- self$predictor_preparator$Pheno[[trait]]
      ETA <- self$predictor_preparator$X

      EnvMatrix <- ETA$Env$x
      LineMatrix <- ETA$Line$x

      V11 <- self$Variance[fold$testing, fold$testing, drop = FALSE]
      V21 <- self$Variance[-fold$testing, fold$testing, drop = FALSE]
      V12 <- t(V21)
      V22 <- self$Variance[-fold$testing, -fold$testing, drop = FALSE]

      # The iverse of the covariance matrix correponding to the training set
      V22inv <- V22 - V21 %*% solve(V11) %*% t(V21)

      # Matrix of the covariances for the testing brreding values
      G <- LineMatrix[fold$testing, -fold$testing, drop = FALSE] +
        EnvMatrix[fold$testing, -fold$testing, drop = FALSE]

      if ("envxline" %in% self$predictor_preparator$predictors) {
        LineEnvMatrix <- ETA$EnvxLine$x
        G <- G + LineEnvMatrix[fold$testing, -fold$testing, drop = FALSE]
      }

      # Prediction of the breeding values for the testing set
      # Formula 21 of the paper: ui = g · V22inv · Y*;
      # where Y* = Y - mean(Y)
      u_testing <- G %*% V22inv %*%
        as.matrix(y[-fold$testing] - mean(y[-fold$testing]))

      return(mean(y) + as.numeric(u_testing))
    },

    eval_multitrait_fold = function(fold) {
      if (is.null(self$model)) {
        ETA <- self$predictor_preparator$X
        y <- self$predictor_preparator$Pheno %>%
          select(all_of(self$traits))

        self$model <- bayesian_model(
          x = ETA,
          y = y,

          iterations_number = self$iterations_number,
          burn_in = self$burn_in,
          thinning = self$thinning,

          verbose = FALSE
        )

        self$Variance <- self$get_multitrait_variance(self$model)
      }

      return(self$model)
    },
    get_multitrait_variance = function(model) {
      Pheno <- self$predictor_preparator$Pheno
      ETA <- self$predictor_preparator$X

      rows_num <- nrow(Pheno)
      EnvVar <- model$fitted_model$ETA$Env$Cov$Omega
      LineVar <- model$fitted_model$ETA$Line$Cov$Omega
      ErrorVar <- model$fitted_model$resCov$R

      ## V: Covariance matrix of the trait y
      EnvMatrix <- ETA$Env$x
      LineMatrix <- ETA$Line$x

      # Kornecker product
      V <- EnvMatrix %x% EnvVar +
        LineMatrix %x% LineVar +
        diag(1, rows_num) %x% ErrorVar

      if ("envxline" %in% self$predictor_preparator$predictors) {
        LineEnvMatrix <- ETA$EnvxLine$x
        LineEnvVar <- model$fitted_model$ETA$EnvxLine$Cov$Omega
        V <- V + LineEnvMatrix %x% LineEnvVar
      }

      return(solve(V))
    },
    predict_multitrait = function(model, fold) {
      Pheno <- self$predictor_preparator$Pheno
      y <- Pheno %>%
        select(all_of(self$traits)) %>%
        as.matrix()
      rows_num <- nrow(Pheno)
      ETA <- self$predictor_preparator$X
      EnvMatrix <- self$predictor_preparator$X$Env$x
      LineMatrix <- self$predictor_preparator$X$Line$x
      traits_number <- length(self$traits)
      Variance <- self$Variance
      EnvVar <- model$fitted_model$ETA$Env$Cov$Omega
      LineVar <- model$fitted_model$ETA$Line$Cov$Omega
      ErrorVar <- model$fitted_model$resCov$R

      # Original testing positions:
      testing_indices <- traits_number * fold$testing

      # New testing positions, when Y is flatten into a vector:
      for (k in seq(traits_number - 1)) {
        testing_indices <- c(testing_indices, traits_number * fold$testing - k)
      }
      testing_indices <- sort(testing_indices)

      # Partitioned V
      V11 <- Variance[testing_indices, testing_indices, drop = FALSE]
      V21 <- Variance[-testing_indices, testing_indices, drop = FALSE]
      V12 <- t(V21)
      V22 <- Variance[-testing_indices, -testing_indices, drop = FALSE]

      # The iverse of the covariance matrix correponding to the training set
      V22inv <- V22 - V21 %*% solve(V11) %*% t(V21)

      # Matrix of the covariances for the testing brreding values
      M <- matrix(rep(1, traits_number^2), ncol = traits_number)
      g <- (LineMatrix %x% LineVar)[testing_indices, -testing_indices] +
        (EnvMatrix %x% EnvVar)[testing_indices, -testing_indices]

      if ("envxline" %in% self$predictor_preparator$predictors) {
        LineEnvMatrix <- ETA$EnvxLine$x
        LineEnvVar <- model$fitted_model$ETA$EnvxLine$Cov$Omega
        LineEnvTerm <- (LineEnvMatrix %x% LineEnvVar)[
          testing_indices,
          -testing_indices
        ]
        g <- g + LineEnvTerm
      }

      # Prediction of the breeding values for the testing set
      #  Formula 21 of the paper: ui = g · V22inv · Y*;
      #   where Y* = Y - mean(Y)
      bv <- y[-fold$testing, ] -
        as.matrix(rep(1, rows_num - length(fold$testing))) %*%
        apply(y[-fold$testing, ], 2, mean)

      u_tst_i <- g %*% V22inv %*% t(t(as.vector(t(bv))))

      # Predictions rearranged in a matricial form
      Predicted <- as.matrix(rep(1, length(fold$testing))) %*%
        apply(y[-fold$testing, ], 2, mean) +
        matrix(u_tst_i, ncol = length(self$traits), byrow = TRUE)
      colnames(Predicted) <- self$traits

      return(as_tibble(Predicted))
    },
    export = function() {
      results <- super$export()
      results$model <- self$model
      results$model_name <- self$model_name
      results$iterations_number <- self$iterations_number
      results$burn_in <- self$burn_in
      results$thinning <- self$thinning

      class(results) <- "GSFastBayesian"

      return(results)
    }
  )
)

#' @export
print.GSFastBayesian <- function(model) {
  print.GSCrossEvaluator(model)
  echo("\n$model_name: %s", set_collapse(model$model_name))
  echo("\n$iterations_number: %i", model$iterations_number)
  echo("\n$burn_in: %i", model$burn_in)
  echo("\n$thinning: %i", model$thinning)
}
