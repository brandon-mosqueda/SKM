#' @import dplyr

#' @importFrom R6 R6Class
#' @importFrom tibble tibble

#' @include model.R
#' @include bayesian_model.R
#' @include model_helpers.R
#' @include geno_preparator.R
#' @include markers_preparator.R

GSRadialModel <- R6Class(
  classname = "GSRadialModel",
  inherit = BayesianModel,
  public = list(
    # Properties --------------------------------------------------

    Pheno = NULL,
    predictors = NULL,

    geno_preparator = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...,
                          lines,
                          envs,
                          Geno,
                          Markers,
                          predictors,
                          rho,
                          tune_type) {
      super$initialize(
        ...,
        x = NULL,
        tune_type = tune_type,
        covariance_structure = NULL,
        records_weights = NULL,
        response_groups = NULL
      )

      self$name <- "Radial Bayesian Model"
      self$Pheno <- tibble(Line = lines, Env = envs)

      if (is.null(Geno)) {
        self$geno_preparator <- MarkersPreparator$new(Markers)
      } else {
        self$geno_preparator <- GenoPreparator$new(Geno)
      }
      self$predictors <- format_predictors(predictors)

      self$fit_params$rho <- rho
      if (is_bayesian_tuner(self$tuner_class)) {
        self$fit_params$rho <- format_bayes_hyperparam(self$fit_params$rho)
      }

      self$tuner_class <- get_tuner(sprintf("radial_%s", tune_type))
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_x = function() { },
    handle_nas = function() {
      self$fit_params$testing_indices <- union(
        self$fit_params$testing_indices,
        which_is_na(self$y)
      )
    },
    prepare_others = function() {
      if (!is_empty(self$fit_params$testing_indices)) {
        if (self$is_multivariate) {
          self$y[self$fit_params$testing_indices, ] <- NA
        } else {
          self$y[self$fit_params$testing_indices] <- NA
        }
      }
    },
    get_hyperparams = function() {
      return(list(rho = self$fit_params$rho))
    },

    has_to_tune = function() {return(is_hyperparam(self$fit_params$rho))},
    tune = function() {
      if (private$has_to_tune()) {
        PhenoTune <- self$Pheno
        GenoTune <- self$Geno
        y_tune <- self$y
        geno_tune_preparator <- self$geno_preparator$clone()

        if (!is_empty(self$fit_params$testing_indices)) {
          PhenoTune <- droplevels(get_records(
            PhenoTune,
            -self$fit_params$testing_indices
          ))
          y_tune <- get_records(
            y_tune,
            -self$fit_params$testing_indices
          )
        }

        hyperparams <- private$get_hyperparams()

        tuner <- self$tuner_class$new(
          Pheno = PhenoTune,
          y = y_tune,
          geno_preparator = geno_tune_preparator,
          predictors = self$predictors,

          responses = self$responses,
          is_multivariate = self$is_multivariate,
          hyperparams = hyperparams,
          fit_params = self$fit_params,
          cv_type = self$tune_cv_type,
          folds_number = self$tune_folds_number,
          testing_proportion = self$tune_testing_proportion,
          folds = self$tune_folds,
          loss_function = self$tune_loss_function,
          grid_proportion = self$tune_grid_proportion,

          iterations_number = self$tune_bayes_iterations_number,
          samples_number = self$tune_bayes_samples_number,

          model_iterations_number = self$fit_params$iterations_number,
          burn_in = self$fit_params$burn_in,
          thinning = self$fit_params$thinning
        )

        tuner$tune()
        self$best_hyperparams <- tuner$best_combination
        self$hyperparams_grid <- tuner$all_combinations
        self$tune_folds <- tuner$cross_validator$folds
      } else {
        self$hyperparams_grid <- data.frame()
        self$best_hyperparams <- list()
      }
    },

    train = function(...) {
      super$train(..., predictors = self$predictors)
    },

    train_univariate = train_radial_bayes,

    predict_univariate = function(indices) {
      return(predict(self$fitted_model, indices))
    },
    coefficients_univariate = function() {
      return(coef(self$fitted_model))
    },

    train_multivariate = train_radial_bayes,
    predict_multivariate = function(indices) {
      return(predict(self$fitted_model, indices))
    },
    coefficients_multivariate = function() {
      return(coef(self$fitted_model))
    }
  )
)
