#' @import dplyr

#' @importFrom R6 R6Class

#' @include utils.R
#' @include gs_cross_evaluator.R
#' @include bayesian_model.R

GSFastBayesianCrossEvaluator <- R6Class(
  classname = "GSFastBayesianCrossEvaluator",
  inherit = GSCrossEvaluator,
  public = list(
    # Properties ---------------------------------------------------------------

    model = NULL,
    Variance = NULL,

    iterations_number = NULL,
    burn_in = NULL,
    thinning = NULL
  ),
  private = list(
    # Methods ------------------------------------------------------------------

    eval_unitrait_fold = function(trait, fold) {
      if (is.null(self$model)) {
        ETA <- self$predictor_preparator$X

        self$model <- bayesian_model(
          x = ETA,
          y = self$predictor_preparator$Pheno[[trait]],

          iterations_number = self$iterations_number,
          burn_in = self$burn_in,
          thinning = self$thinning,

          verbose = FALSE
        )

        self$Variance <- self$get_unitrait_variance(self$model$fitted_model)
      }

      return(self$model)
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

        self$Variance <- self$get_multitrait_variance(self$model$fitted_model)
      }

      return(self$model)
    },
    get_unitrait_variance = function(model) {
      Pheno <- self$predictor_preparator$Pheno
      ETA <- self$predictor_preparator$X

      # Variance components
      rows_num <- nrow(Pheno)

      EnvVar <- model$ETA$Env$varU
      LineVar <- model$ETA$Line$varU
      ErrorVar <- model$varE

      ## V: Covariance matrix of the trait y
      EnvMatrix <- ETA$Env$x
      LineMatrix <- ETA$Line$x
      envs_num <- length(unique(Pheno$Env))
      V <- (envs_num * EnvMatrix * EnvVar) +
        (LineMatrix * LineVar + diag(ErrorVar, rows_num))

      if ("linexenv" %in% self$predictor_preparator$predictors) {
        LineEnvMatrix <- ETA$LinexEnv$x
        LineEnvVar <- model$ETA$GE$varU
        V <- V + envs_num * LineEnvMatrix * LineEnvVar
      }

      return(solve(V))
    },
    predict_unitrait = function(trait, model, fold) {
      y <- self$predictor_preparator$Pheno[[trait]]
      EnvMatrix <- self$predictor_preparator$X$Env$x
      LineMatrix <- self$predictor_preparator$X$Line$x

      V11 <- self$Variance[fold$testing, fold$testing, drop = FALSE]
      V21 <- self$Variance[-fold$testing, fold$testing, drop = FALSE]
      V12 <- t(V21)
      V22 <- self$Variance[-fold$testing, -fold$testing, drop = FALSE]

      # The iverse of the covariance matrix correponding to the training set
      V22inv <- V22 - V21 %*% solve(V11) %*% t(V21)

      # Matrix of the covariances for the testing brreding values
      G <- LineMatrix[fold$testing, -fold$testing, drop = FALSE] +
        EnvMatrix[fold$testing, -fold$testing, drop = FALSE]

      if ("linexenv" %in% self$predictor_preparator$predictors) {
        LineEnvMatrix <- ETA$LinexEnv$x
        G <- G + LineEnvMatrix[fold$testing, -fold$testing, drop = FALSE]
      }

      # Prediction of the breeding values for the testing set
      # Formula 21 of the paper: ui = g · V22inv · Y*;
      # where Y* = Y - mean(Y)
      u_testing <- G %*% V22inv %*%
        as.matrix(y[-fold$testing] - mean(y[-fold$testing]))

      return(mean(y[-fold$testing]) + u_testing)
    },
    get_multitrait_variance = function(model) {
      Pheno <- self$predictor_preparator$Pheno
      ETA <- self$predictor_preparator$X

      rows_num <- nrow(Pheno)
      EnvVar <- model$ETA$Env$Cov$Omega
      LineVar <- model$ETA$Line$Cov$Omega
      ErrorVar <- model$resCov$R

      ## V: Covariance matrix of the trait y
      EnvMatrix <- ETA$Env$x
      LineMatrix <- ETA$Line$x

      # Kornecker product
      V <- EnvMatrix %x% EnvVar +
        LineMatrix %*% LineVar +
        diag(1, rows_num) %x% ErrorVar

      if ("linexenv" %in% self$predictor_preparator$predictors) {
        LineEnvMatrix <- ETA$LinexEnv$x
        LineEnvVar <- model$ETA$GE$Cov$Omega
        V <- V + LineEnvMatrix %x% LineEnvVar
      }

      return(solve(V))
    },
    predict_multitrait = function(model, fold) {
      Pheno <- self$predictor_preparator$Pheno
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

      if ("linexenv" %in% self$predictor_preparator$predictors) {
        LineEnvMatrix <- ETA$LinexEnv$x
        LineEnvVar <- model$fitted_model$ETA$GE$Cov$Omega
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
        matrix(u_tst_i, ncol = nt, byrow = T)
      colnames(Predicted) <- self$traits

      return(as_tibble(Predicted))
    }
  )
)
