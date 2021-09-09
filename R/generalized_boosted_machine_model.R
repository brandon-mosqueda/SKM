#' @importFrom R6 R6Class
#' @importFrom gbm gbm

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

                          records_weights,
                          predictors_relationship) {
      super$initialize(
        ...,
        name = "Generalized Boosted Machine",
        is_multivariate = FALSE,
        is_x_matrix = FALSE
      )

      self$hyperparams$trees_number <- trees_number
      self$hyperparams$max_depth <- max_depth
      self$hyperparams$node_size <- node_size
      self$hyperparams$shrinkage <- shrinkage
      self$hyperparams$sampled_records_proportion <- sampled_records_proportion

      self$other_params$records_weights <- records_weights
      self$other_params$predictors_relationship <- predictors_relationship
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_others = function() {
      if (is_binary_response(self$responses$y$type)) {
        self$y <- ifelse(self$y == self$responses$y$levels[1], 1, 0)
      }

      self$other_params$distribution <- get_gbm_distribution(
        self$responses$y$type
      )

      self$other_params$predictors_relationship <- remove_if_has_more(
        self$other_params$predictors_relationship,
        ncol(self$x),
        self$removed_x_cols
      )

      self$other_params$records_weights <- remove_if_has_more(
        self$other_params$records_weights,
        nrow(self$x),
        self$removed_rows
      )
    },
    get_x_for_model = function(x, remove_cols = FALSE) {
      return(to_data_frame(x))
    },

    tune = function() {
      true_other_params <- self$other_params
      self$other_params$records_weights <- NULL

      super$tune()

      self$other_params <- true_other_params
    },

    train_univariate = function(x, y, hyperparams, other_params) {
      data <- data.frame(y, x)

      model <- suppressMessages(gbm(
        formula = y ~ .,

        data = data,

        n.trees = hyperparams$trees_number,
        interaction.depth = hyperparams$max_depth,
        n.minobsinnode = hyperparams$node_size,
        shrinkage = hyperparams$shrinkage,
        bag.fraction = hyperparams$sampled_records_proportion,

        distribution = other_params$distribution,
        weights = other_params$records_weights,
        var.monotone = other_params$predictors_relationship,

        train.fraction = 1,
        verbose = FALSE,
        keep.data = FALSE
      ))

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  other_params,
                                  hyperparams) {
      predictions <- hush(
        predict(
          model,
          newdata = x,
          # type response returns the probabilites for categorical data and counts
          # for poisson data, for continuous type link and response returns the
          # same
          type = "response"
        ),
        all = TRUE
      )

      if (is_binary_response(responses$y$type)) {
        # Predictions are only the probabilities of being 1 (response level 1)
        probabilities <- cbind(predictions, 1 - predictions)
        colnames(probabilities) <- responses$y$levels
        predictions <- ifelse(
          predictions > 0.5,
          responses$y$levels[1],
          responses$y$levels[2]
        )
        predictions <- factor(predictions, levels = responses$y$levels)

        predictions <- list(
          predicted = predictions,
          probabilities = as.data.frame(probabilities)
        )
      } else if (is_class_response(responses$y$type)) {
        probabilities <- as.data.frame(predictions[, , 1])
        classes <- colnames(predictions)

        predictions <- classes[apply(predictions, 1, which.max)]
        predictions <- factor(predictions, levels = responses$y$levels)

        predictions <- list(
          predicted = predictions,
          probabilities = probabilities
        )
      } else {
        predictions <- list(predicted = predictions)
      }

      return(predictions)
    }
  )
)
