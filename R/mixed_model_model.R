#' @importFrom R6 R6Class
#' @importFrom nlme fixef

#' @include lme4GS.R
#' @include model.R
#' @include model_helpers.R

MixedModel <- R6Class(
  classname = "MixedModel",
  inherit = Model,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(..., testing_indices) {
      super$initialize(
        ...,
        name = "Mixed Model",
        allow_coefficients = FALSE,
        is_x_matrix = FALSE
      )

      self$fit_params$testing_indices <- testing_indices
      self$fit_params$names_data <- data.frame()
    },

    # Methods --------------------------------------------------

    # Since this model does not perform hyperparameter tuning and holds the
    # predictions in the same model data, other predict function definition
    # is needed.
    predict = function(indices = NULL) {
      private$predict_univariate(indices)
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_x = function() {
      x_names <- names(self$x)
      if (is.null(x_names)) {
        x_names <- paste0("x_", seq_along(self$x))
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

      # Column and row names correction
      for (i in 1:length(self$x)) {
        row_names <- make.names(rownames(self$x[[i]]$x), unique = TRUE)
        rownames(self$x[[i]]$x) <- row_names
        # It is expected all are square matrices
        colnames(self$x[[i]]$x) <- row_names
      }
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

      self$fit_params$names_data <- list()

      for (name in names(self$x)) {
        # Rename x to K, the expected format for lmerUvcov
        self$x[[name]]$K <- self$x[[name]]$x
        self$x[[name]]$x <- NULL

        self$fit_params$names_data[[name]] <- rownames(self$x[[name]]$K)
      }
      self$fit_params$names_data <- as.data.frame(self$fit_params$names_data)

      self$fit_params$names_data$y <- self$y
      self$fit_params$formula <- build_mixed_formula(names(self$x))
      self$fit_params$training_indices <- setdiff(
        seq_along(self$y),
        self$fit_params$testing_indices
      )
    },

    has_to_tune = function() return(FALSE),

    train_univariate = function(x, y, fit_params) {
      training_indices <- self$fit_params$training_indices

      names_data <- self$fit_params$names_data[training_indices, , drop = FALSE]
      uvcov <- list()
      for (name in names(x)) {
        uvcov[[name]] <- list(
          K = x[[name]]$K[training_indices, training_indices, drop = FALSE]
        )
      }

      model <- lmerUvcov(
        self$fit_params$formula,
        data = names_data,
        Uvcov = uvcov,
        verbose = FALSE
      )

      return(model)
    },

    prepare_predict_indices = function(indices) {
      if (is.null(self$fit_params$testing_indices)) {
        if (is_empty(indices)) {
          stop(
            "Error in predicting. With mixed models you need to provide ",
            "the testing_indices parameter when calling mixed_model ",
            "function in order to make predictions, set some values in the ",
            "response variable as NA or send the indices parameter in predict ",
            "function"
          )
        }
      } else if (is_empty(indices)) {
        indices <- self$fit_params$testing_indices
      }

      return(indices)
    },

    predict_univariate = function(indices) {
      indices <- private$prepare_predict_indices(indices)

      # Here we are using the full x data, not only the training. Internally,
      # lmerUvCov will find the correct values for the testing indices.
      testing_predictions <- ranefUvcovNew(self$fitted_model, Uvcov = self$x)

      # As the method used by ranefUvcovNew to find testing indices could return
      # the predictions in a different order, we need to match the indices
      first_name <- names(testing_predictions)[1]
      match_indices <- match(
        self$fit_params$names_data[[first_name]][indices],
        rownames(testing_predictions[[first_name]])
      )

      predictions <- rep(0, length(indices))
      for (name in names(self$x)) {
        predictions <- predictions + testing_predictions[[name]][
          match_indices,
          1
        ]
      }

      predictions <- nlme::fixef(self$fitted_model)[1] + predictions

      return(list(predicted = predictions))
    }
  )
)

#' @title Predict Mixed model
#'
#' @description
#' Obtains the predictions using a fitted model object of class `MixedModel`.
#'
#' @inheritParams predict.Model
#' @param indices (`numeric`) A numeric vector with the indices of the elements
#'   used to fit the model you want the predictions. `NULL` by default which
#'   uses the indices specified in `testing_indices` when the model was fitted
#'   or those elements with `NA` values.
#'
#' @template return-predict
#'
#' @example inst/examples/mixed_model.R
#'
#' @export
predict.MixedModel <- function(model, indices = NULL, format = "list") {
  predictions <- model$predict(indices)

  return(format_predictions(
    predictions,
    is_multivariate = FALSE,
    format = format
  ))
}
