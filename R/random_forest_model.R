#' @importFrom R6 R6Class
#' @importFrom randomForestSRC rfsrc vimp get.mv.vimp

#' @include utils.R
#' @include model.R
#' @include model_helpers.R

RandomForestModel <- R6Class(
  classname = "RandomForestModel",
  inherit = Model,
  public = list(
    # Properties --------------------------------------------------

    is_regression_model = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...,
                          trees_number,
                          node_size,
                          node_depth,
                          sampled_x_vars_number,

                          split_rule,
                          splits_number,
                          x_vars_weights,
                          records_weights,
                          na_action) {
      super$initialize(
        ...,
        name = "Random Forest",
        allow_coefficients = TRUE
      )

      self$fit_params$trees_number <- trees_number
      self$fit_params$node_size <- node_size
      self$fit_params$node_depth <- node_depth
      self$fit_params$sampled_x_vars_number <- sampled_x_vars_number

      if (is.null(split_rule)) {
        self$fit_params$split_rule <- NULL
      } else {
        self$fit_params$split_rule <- tolower(split_rule)
      }
      self$fit_params$splits_number <- splits_number
      self$fit_params$x_vars_weights <- x_vars_weights
      self$fit_params$records_weights <- records_weights
      self$fit_params$na_action <- prepare_random_forest_na_action(na_action)
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
        self$fit_params$node_size <- format_bayes_hyperparam(
          self$fit_params$node_size,
          is_int = TRUE
        )
        self$fit_params$node_depth <- format_bayes_hyperparam(
          self$fit_params$node_depth,
          is_int = TRUE
        )

        if (is.list(self$fit_params$sampled_x_vars_number)) {
          self$fit_params$sampled_x_vars_number$min <- proportion_to(
            self$fit_params$sampled_x_vars_number$min,
            ncol(self$x)
          )
          self$fit_params$sampled_x_vars_number$max <- proportion_to(
            self$fit_params$sampled_x_vars_number$max,
            ncol(self$x)
          )
        } else {
          self$fit_params$sampled_x_vars_number <- proportion_to(
            self$fit_params$sampled_x_vars_number,
            ncol(self$x)
          )
        }

        self$fit_params$sampled_x_vars_number <- format_bayes_hyperparam(
          self$fit_params$sampled_x_vars_number,
          is_int = TRUE
        )
      } else {
        self$fit_params$sampled_x_vars_number <- proportion_to(
          self$fit_params$sampled_x_vars_number,
          ncol(self$x)
        )
      }

      if (self$is_multivariate) {
        self$is_regression_model <- all(sapply(
          self$responses,
          function(x) is_numeric_response(x$type)
        ))
      }

      self$fit_params$model_formula <- get_random_forest_formula(
        self$responses,
        self$is_multivariate,
        self$is_regression_model
      )

      self$fit_params$x_vars_weights <- remove_if_has_more(
        self$fit_params$x_vars_weights,
        ncol(self$x),
        self$removed_x_cols
      )

      self$fit_params$records_weights <- remove_if_has_more(
        self$fit_params$records_weights,
        nrow(self$x),
        self$removed_rows
      )
    },
    get_x_for_model = function(x, remove_cols = FALSE) {
      return(to_data_frame(x))
    },
    handle_nas = function() {
      if (has_str(self$fit_params$na_action, "omit")) {
        super$handle_nas()
      }
    },

    tune = function() {
      true_other_params <- self$fit_params
      self$fit_params$records_weights <- NULL

      super$tune()

      self$fit_params <- true_other_params
    },

    train_univariate = train_random_forest,
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  fit_params) {
      x <- data.frame(x, check.names = FALSE)
      predictions <- predict(model, newdata = x)

      if (is_class_response(responses$y$type)) {
        predictions <- list(
          predicted = predictions$class,
          probabilities = as.data.frame(predictions$predicted)
        )
      } else {
        predictions <- list(predicted = predictions$predicted)
      }

      return(predictions)
    },
    coefficients_univariate = function() {
      coefs <- vimp(self$fitted_model)$importance

      if (is_class_response(self$responses$y$type)) {
        coefs <- t(coefs)
      }

      return(coefs)
    },

    train_multivariate = train_random_forest,
    predict_multivariate = function(model,
                                    x,
                                    responses,
                                    fit_params) {
      x <- data.frame(x, check.names = FALSE)
      all_predictions <- predict(model, newdata = x)
      predictions <- list()

      for (response_name in names(responses)) {
        response_type <- responses[[response_name]]$type

        if (is_class_response(response_type)) {
          response_predictions <- all_predictions$classOutput[[response_name]]

          predictions[[response_name]] <- list(
            predicted = response_predictions$class,
            probabilities = as.data.frame(response_predictions$predicted)
          )
        } else {
          predictions[[response_name]] <- list(
            predicted = all_predictions$regrOutput[[response_name]]$predicted
          )
        }
      }

      return(predictions)
    },
    coefficients_multivariate = function() {
      coefs <- list()
      all_coefs <- get.mv.vimp(self$fitted_model)

      for (name in names(self$responses)) {
        coefs[[name]] <- all_coefs[, name]
      }

      return(coefs)
    }
  )
)
