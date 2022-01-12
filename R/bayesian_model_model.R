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
                          covariance_structure,
                          records_weights,
                          response_groups,
                          testing_indices) {
      super$initialize(
        ...,
        name = "Bayesian Model",
        allow_coefficients = TRUE,
        is_x_matrix = FALSE
      )

      self$fit_params$iterations_number <- iterations_number
      self$fit_params$burn_in <- burn_in
      self$fit_params$thinning <- thinning
      self$fit_params$covariance_structure <- covariance_structure
      self$fit_params$records_weights <- records_weights
      self$fit_params$response_groups <- response_groups
      self$fit_params$testing_indices <- testing_indices
      self$fit_params$trash_dir <- sprintf(
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

      x_names <- names(self$x)
      if (is.null(x_names)) {
        x_names <- paste0("x_", seq(length(self$x)))
      } else {
        i <- 1
        x_names <- sapply(x_names, function(name) {
          if (is_empty(name) || name == "") {
            name <- sprintf("x_%s", i)
            i <<- i + 1
          }

          return(name)
        })
      }

      names(self$x) <- x_names
    },
    get_x_for_model = function(x, ...) {
      return(x)
    },
    handle_nas = function() {
      na_indices <- c()

      for (i in 1:length(self$x)) {
        na_indices <- c(na_indices, which_is_na(self$x[[i]]$x))
      }

      self$fit_params$testing_indices <- c(
        self$fit_params$testing_indices,
        which_is_na(self$y)
      )

      if (!is.null(na_indices)) {
        self$removed_rows <- unique(na_indices)

        for (i in 1:length(self$x)) {
          self$x[[i]]$x <- get_records(self$x[[i]]$x, -self$removed_rows)
        }

        self$y <- get_records(self$y, -self$removed_rows)

        if (!is.null(self$fit_params$testing_indices)) {
          # Change the index to the new positions
          temp <- rep(FALSE, max(self$fit_params$testing_indices))
          temp[self$fit_params$testing_indices] <- TRUE
          temp <- temp[-self$removed_rows]
          self$fit_params$testing_indices <- which(temp)

          if (is_empty(self$fit_params$testing_indices)) {
            self$fit_params$testing_indices <- NULL
            warning(
              "All testing indices were removed due to there are records in X ",
              "that contains NA values"
            )
          }
        }

        warning(
          length(self$removed_rows),
          " rows were removed because it has NA values in x and/or y. ",
          "See model$removed_rows to see what rows were removed."
        )
      }
    },
    prepare_others = function() {
      if (is.null(self$fit_params$testing_indices)) {
        self$fit_params$testing_indices <- which_is_na(self$y)
      } else {
        self$fit_params$testing_indices <- setdiff(
          self$fit_params$testing_indices,
          self$removed_rows
        )

        if (is_empty(self$fit_params$testing_indices)) {
          self$fit_params$testing_indices <- NULL
        }
      }

      if (self$is_multivariate) {
        if (!is.null(self$fit_params$testing_indices)) {
          self$y[self$fit_params$testing_indices, ] <- NA
        }

        self$fit_params$covariance_structure$type <- prepare_covariance_type(
          self$fit_params$covariance_structure$type
        )
      } else {
        self$fit_params$records_weights <- remove_if_has_more(
          self$fit_params$records_weights,
          ncol(self$x[[1]]$x),
          self$removed_x_cols
        )

        self$fit_params$response_groups <- remove_if_has_more(
          self$fit_params$response_groups,
          ncol(self$x[[1]]$x),
          self$removed_x_cols
        )

        self$fit_params$bglr_response_type <- get_bglr_response_type(
          self$responses$y$type
        )

        if (!is.null(self$fit_params$testing_indices)) {
          self$y[self$fit_params$testing_indices] <- NA
        }
      }

      for (i in 1:length(self$x)) {
        x_name <- get_bglr_matrix_param_name(self$x[[i]]$model)
        x_index <- which(names(self$x[[i]]) == "x")
        names(self$x[[i]])[x_index] <- x_name
      }
    },

    has_to_tune = function() return(FALSE),

    train_univariate = function(x, y, fit_params) {
      mkdir(fit_params$trash_dir)

      model <- BGLR(
        y = y,
        response_type = fit_params$bglr_response_type,
        ETA = x,

        nIter = fit_params$iterations_number,
        burnIn = fit_params$burn_in,
        thin = fit_params$thinning,
        weights = fit_params$records_weights,
        groups = fit_params$response_groups,

        verbose = FALSE,
        saveAt = file.path(fit_params$trash_dir, "bayesian_")
      )

      rmdir(fit_params$trash_dir)

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  fit_params) {
      if (is.null(fit_params$testing_indices)) {
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
        predictions <- predictions[fit_params$testing_indices]
        predictions <- factor(predictions, levels = responses$y$levels)

        probabilities <- as.data.frame(
          probabilities[fit_params$testing_indices, ]
        )

        predictions <- list(
          predicted = predictions,
          probabilities = probabilities
        )
      } else {
        predictions <- model$yHat[fit_params$testing_indices]
        if (is.null(predictions)) {
          predictions <- model$ETAHat[fit_params$testing_indices]
        }

        predictions <- list(predicted = predictions)
      }

      return(predictions)
    },
    coefficients_univariate = function() {
      coefs <- list()
      all_coefs <- self$fitted_model$ETA

      for (coef_name in names(all_coefs)) {
        coefs[[coef_name]] <- all_coefs[[coef_name]]$b
      }

      return(coefs)
    },

    train_multivariate = function(x, y, fit_params) {
      mkdir(fit_params$trash_dir)

      model <- hush(Multitrait(
        y = y,
        ETA = x,

        resCov = fit_params$covariance_structure,
        nIter = fit_params$iterations_number,
        burnIn = fit_params$burn_in,
        thin = fit_params$thinning,

        verbose = FALSE,
        saveAt = file.path(fit_params$trash_dir, "bayesian_")
      ))

      rmdir(fit_params$trash_dir)

      return(model)
    },
    predict_multivariate = function(model,
                                    x,
                                    responses,
                                    fit_params) {
      predictions <- list()
      all_predictions <- model$yHat
      if (is.null(all_predictions)) {
        all_predictions <- model$ETAHat
      }

      all_predictions <- all_predictions[fit_params$testing_indices, ]

      for (response_name in names(responses)) {
        predictions[[response_name]] <- list(
          predicted = all_predictions[, response_name]
        )
      }

      return(predictions)
    },
    coefficients_multivariate = function() {
      coefs <- list()
      all_coefs <- self$fitted_model$ETA

      response_i <- 1
      for (response_name in colnames(self$y)) {
        coefs[[response_name]] <- list()

        for (coef_name in names(all_coefs)) {
          coefs[[response_name]][[coef_name]] <-
            all_coefs[[coef_name]]$beta[, response_i]

          names(coefs[[response_name]][[coef_name]]) <- colnames(
            nonull(self$x[[coef_name]]$X, self$x[[coef_name]]$K)
          )
        }

        response_i <- response_i + 1
      }

      return(coefs)
    }
  )
)

#' @export
predict.BayesianModel <- function(model) {
  return(model$predict(NULL))
}
