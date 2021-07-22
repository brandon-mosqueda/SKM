#' @importFrom R6 R6Class
#' @importFrom BGLR BGLR Multitrait

#' @include utils.R
#' @include model.R
#' @include model_helpers.R

BayesianModel <- R6Class(
  classname = "BayesianModel",
  inherit = Model,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(...,
                          iterations_number,
                          burn_in,
                          thinning,
                          records_weights,
                          response_groups,
                          testing_indices) {
      super$initialize(..., name = "Bayesian Model")

      self$other_params$iterations_number <- iterations_number
      self$other_params$burn_in <- burn_in
      self$other_params$thinning <- thinning
      self$other_params$records_weights <- records_weights
      self$other_params$response_groups <- response_groups
      self$other_params$testing_indices <- testing_indices
      self$other_params$trash_dir <- sprintf(
        "%s_%s",
        BAYESIAN_TRASH_DIR,
        as.numeric(Sys.time())
      )
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_multivariate_y = prepare_multivariate_y_only_numeric,
    prepare_x = function() {
      for (i in 1:length(self$x)) {
        self$x[[i]]$x <- remove_no_variance_cols(to_matrix(self$x[[i]]$x))
        self$x[[i]]$model <- prepare_bayesian_model(self$x[[i]]$model)
      }
    },
    handle_nas = function() {
      na_indices <- c()

      for (i in 1:length(self$x)) {
        na_indices <- c(na_indices, which_is_na(self$x[[i]]$x))
      }

      if (!is.null(self$other_params$testing_indices)) {
        na_indices <- c(na_indices, which_is_na(self$y))
      }

      if (!is.null(na_indices)) {
        self$removed_rows <- unique(na_indices)

        self$x <- get_records(self$x, -self$removed_rows)

        if (!is.null(self$other_params$testing_indices)) {
          self$y <- get_records(self$y, -self$removed_rows)
        }

        warning(
          length(self$removed_rows),
          " rows were removed because it has NA values in x and/or y. ",
          "See model$removed_rows to see what rows were removed."
        )
      }
    },
    prepare_others = function() {
      self$other_params$records_weights <- remove_if_has_more(
        self$other_params$records_weights,
        ncol(self$x[[1]]$x),
        self$removed_x_cols
      )

      self$other_params$response_groups <- remove_if_has_more(
        self$other_params$response_groups,
        ncol(self$x[[1]]$x),
        self$removed_x_cols
      )

      if (is.null(self$other_params$testing_indices)) {
        self$other_params$testing_indices <- which_is_na(self$y)
      } else {
        self$other_params$testing_indices <- setdiff(
          self$other_params$testing_indices,
          self$removed_rows
        )

        if (is_empty(self$other_params$testing_indices)) {
          self$other_params$testing_indices <- NULL
        }
      }

      if (self$is_multivariate) {
        if (!is.null(self$other_params$testing_indices)) {
          self$y[self$other_params$testing_indices, ] <- NA
        }
      } else {
        self$other_params$bglr_response_type <- get_bglr_response_type(
          self$responses$y$type
        )

        for (i in 1:length(self$x)) {
          x_name <- get_bglr_matrix_param_name(self$x[[i]]$model)
          x_index <- which(names(self$x[[i]]) == "x")
          names(self$x[[i]])[x_index] <- x_name
        }

        if (!is.null(self$other_params$testing_indices)) {
          self$y[self$other_params$testing_indices] <- NA
        }
      }

    },

    has_to_tune = function() return(FALSE),

    train_univariate = function(x, y, hyperparams, other_params) {
      mkdir(other_params$trash_dir)

      model <- BGLR(
        y = y,
        response_type = other_params$bglr_response_type,
        ETA = x,

        nIter = other_params$iterations_number,
        burnIn = other_params$burn_in,
        thin = other_params$thinning,
        weights = other_params$records_weights,
        groups = other_params$response_groups,

        verbose = FALSE,
        saveAt = file.path(other_params$trash_dir, "bayesian_")
      )

      rmdir(other_params$trash_dir)

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  other_params,
                                  hyperparams) {
      if (is.null(other_params$testing_indices)) {
        stop(
          "Error in predicting. With bayesian models you need to provide the ",
          "testing_indices parameter when calling bayesian_model function ",
          "in order to make predictions."
        )
      }

      if (is_class_response(responses$y$type)) {
        probabilities <- model$probs
        classes <- colnames(probabilities)

        predictions_cols <- apply(probabilities, 1, which.max)
        predictions <- classes[predictions_cols]
        predictions <- predictions[other_params$testing_indices]
        predictions <- factor(predictions, levels = responses$y$levels)

        probabilities <- as.data.frame(
          probabilities[other_params$testing_indices, ]
        )

        predictions <- list(
          predicted = predictions,
          probabilities = probabilities
        )
      } else {
        predictions <- model$yHat[other_params$testing_indices]
        if (is.null(predictions)) {
          predictions <- model$ETAHat[other_params$testing_indices]
        }

        predictions <- list(predicted = predictions)
      }

      return(predictions)
    },

    train_multivariate = function() {

    },
    predict_multivariate = function(model,
                                    x,
                                    responses,
                                    other_params,
                                    hyperparams) {

    }
  )
)

#' @export
predict.BayesianModel <- function(model) {
  return(model$predict(
    model = model$fitted_model,
    x = NULL,
    responses = model$responses,
    other_params = model$other_params,
    hyperparams = model$best_hyperparams
  ))
}
