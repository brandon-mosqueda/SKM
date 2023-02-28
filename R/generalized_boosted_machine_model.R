#' @importFrom R6 R6Class
#' @importFrom gbm gbm.fit

#' @include utils.R
#' @include model.R
#' @include model_helpers.R

GeneralizedBoostedMachineModel <- R6Class(
  classname = "GeneralizedBoostedMachineModel",
  inherit = Model,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(...,
                          trees_number,
                          max_depth,
                          node_size,
                          shrinkage,
                          sampled_records_proportion,

                          predictors_relationship) {
      super$initialize(
        ...,
        name = "Generalized Boosted Machine",
        is_multivariate = FALSE
      )

      self$fit_params$trees_number <- trees_number
      self$fit_params$max_depth <- max_depth
      self$fit_params$node_size <- node_size
      self$fit_params$shrinkage <- shrinkage
      self$fit_params$sampled_records_proportion <- sampled_records_proportion

      self$fit_params$predictors_relationship <- predictors_relationship
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_others = function() {
      if (is_bayesian_tuner(self$tuner_class)) {
        self$fit_params$trees_number <- format_bayes_hyperparam(
          self$fit_params$trees_number,
          is_int = TRUE
        )
        self$fit_params$max_depth <- format_bayes_hyperparam(
          self$fit_params$max_depth,
          is_int = TRUE
        )
        self$fit_params$node_size <- format_bayes_hyperparam(
          self$fit_params$node_size,
          is_int = TRUE
        )
        self$fit_params$shrinkage <- format_bayes_hyperparam(
          self$fit_params$shrinkage,
          is_int = FALSE
        )
        self$fit_params$sampled_records_proportion <- format_bayes_hyperparam(
          self$fit_params$sampled_records_proportion,
          is_int = FALSE
        )
      }

      if (is_binary_response(self$responses$y$type)) {
        self$y <- as.integer(self$y) - 1
      }

      self$fit_params$distribution <- get_gbm_distribution(
        self$responses$y$type
      )

      self$fit_params$predictors_relationship <- remove_if_has_more(
        self$fit_params$predictors_relationship,
        ncol(self$x),
        self$removed_x_cols
      )
    },

    tune = function() {
      true_other_params <- self$fit_params

      super$tune()

      self$fit_params <- true_other_params
    },

    train_univariate = function(x, y, fit_params) {
      model <- suppressMessages(gbm::gbm.fit(
        x = x,
        y = y,

        n.trees = fit_params$trees_number,
        interaction.depth = fit_params$max_depth,
        n.minobsinnode = fit_params$node_size,
        shrinkage = fit_params$shrinkage,
        bag.fraction = fit_params$sampled_records_proportion,

        distribution = fit_params$distribution,
        var.monotone = fit_params$predictors_relationship,

        verbose = FALSE,
        keep.data = FALSE
      ))

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  fit_params) {
      predictions <- hush(
        predict(
          model,
          newdata = x,
          # type response returns the probabilites for categorical data and
          # counts for poisson data, for continuous type link and response
          # returns the same
          type = "response"
        ),
        all = TRUE
      )

      if (is_class_response(responses$y$type)) {
        if (is_binary_response(responses$y$type)) {
          # Predictions are only the probabilities of being 1 (response level 2)
          probabilities <- cbind(1 - predictions, predictions)
          colnames(probabilities) <- responses$y$levels
          probabilities <- as.data.frame(probabilities)
        } else {
          probabilities <- as.data.frame(predictions[, , 1])
        }

        predictions <- predict_class(probabilities, responses$y)
      } else {
        predictions <- list(predicted = predictions)
      }

      return(predictions)
    }
  )
)
