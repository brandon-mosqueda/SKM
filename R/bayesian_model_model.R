#' @importFrom R6 R6Class
#' @importFrom BGLR BGLR Multitrait

#' @include utils.R
#' @include model.R
#' @include model_helpers.R
#' @include validator.R

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
    },

    # Methods --------------------------------------------------

    # Since this model does not perform hyperparameter tuning and holds the
    # predictions in the same model data, other predict function definition
    # is needed.
    predict = function(indices) {
      if (self$is_multivariate) {
        private$predict_multivariate(indices)
      } else {
        private$predict_univariate(indices)
      }
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_multivariate_y = prepare_multivariate_y_only_numeric,
    prepare_x = function() {
      for (i in 1:length(self$x)) {
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
        saveAt = file.path(tempdir(check = TRUE), as.numeric(Sys.time()))
      )

      return(model)
    },

    predict_univariate = function(indices) {
      if (is_class_response(self$responses$y$type)) {
        probabilities <- as.data.frame(self$fitted_model$probs[indices, ])

        predictions <- predict_class(
          probabilities,
          self$responses$y
        )
      } else {
        predictions <- self$fitted_model$yHat[indices]
        if (is.null(predictions)) {
          predictions <- self$fitted_model$ETAHat[indices]
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
      model <- hush(Multitrait(
        y = y,
        ETA = x,

        resCov = fit_params$covariance_structure,
        nIter = fit_params$iterations_number,
        burnIn = fit_params$burn_in,
        thin = fit_params$thinning,

        verbose = FALSE,
        saveAt = file.path(tempdir(check = TRUE), as.numeric(Sys.time()))
      ))

      return(model)
    },
    predict_multivariate = function(indices) {
      predictions <- list()
      all_predictions <- self$fitted_model$yHat
      if (is.null(all_predictions)) {
        all_predictions <- self$fitted_model$ETAHat
      }

      all_predictions <- all_predictions[indices, ]

      for (response_name in names(self$responses)) {
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

#' @title Predict Bayesian model
#'
#' @description
#' Obtains the predictions using a fitted model object of class `BayesianModel`.
#'
#' @inheritParams predict.Model
#' @param indices (`numeric`) A numeric vector with the indices of the elements
#'   used to fit the model you want the predictions.
#'
#' @template return-predict
#'
#' @examples
#' \dontrun{
#' # Univariate analysis -------------------------------------------------------
#' x <- list(list(x = to_matrix(iris[, -5]), model = "BRR"))
#' y <- iris$Species
#' model <- bayesian_model(x, y, testing_indices = c(1:5, 51:55, 101:105))
#'
#' # Predict using the fitted model (of the specified testing indices)
#' predictions <- predict(model, indices = c(1:5, 51:55, 101:105))
#' # Obtain the predicted values
#' predictions$predicted
#' # Obtain the predicted probabilities
#' predictions$probabilities
#'
#' # Predict using the fitted model (with different indices)
#' predictions <- predict(model, indices = 1:50)
#' predictions$predicted
#'
#' # Multivariate analysis -----------------------------------------------------
#' x <- list(list(x = to_matrix(iris[, -c(1, 2)]), model = "fixed"))
#' y <- iris[, c(1, 2)]
#' y[c(5, 55, 105), 1] <- NA
#' y[c(6, 66, 106), 2] <- NA
#' model <- bayesian_model(x, y, iterations_number = 1000)
#'
#' # Predict using the fitted model, with different indices and data.frame
#' # format
#' predictions <- predict(model, indices = c(10, 20, 30), format = "data.frame")
#' head(predictions)
#' }
#'
#' @export
predict.BayesianModel <- function(model, indices, format = "list") {
  if (missing(indices)) {
    stop("The indices parameter is required")
  }

  assert_testing_indices(
    indices,
    max_length = get_length(model$y),
    required = TRUE
  )
  predictions <- model$predict(indices)

  return(format_predictions(predictions, model$is_multivariate, format))
}
