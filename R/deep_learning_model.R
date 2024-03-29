#' @importFrom R6 R6Class

#' @import keras

#' @include utils.R
#' @include model.R
#' @include globals.R
#' @include deep_learning_grid_tuner.R
#' @include deep_learning_bayesian_tuner.R
#' @include model_helpers.R

DeepLearningModel <- R6Class(
  classname = "DeepLearningModel",
  inherit = Model,
  public = list(
    # Properties --------------------------------------------------

    linear_model = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...,
                          tune_type,
                          is_multivariate,

                          learning_rate,
                          epochs_number,
                          batch_size,
                          layers,
                          output_penalties,

                          optimizer,
                          loss_function,
                          with_platt_scaling,
                          platt_proportion,
                          shuffle,
                          early_stop,
                          early_stop_patience) {
      tune_type <- paste0("deep_", tune_type)

      super$initialize(
        ...,
        tune_type = tune_type,
        is_multivariate = is_multivariate,
        name = "Deep Learning"
      )

      self$fit_params$learning_rate <- learning_rate
      self$fit_params$epochs_number <- epochs_number
      self$fit_params$batch_size <- batch_size
      self$fit_params$output_ridge_penalty <- nonull(
        output_penalties$ridge_penalty,
        DEFAULT_RIDGE_PENALTY
      )
      self$fit_params$output_lasso_penalty <- nonull(
        output_penalties$lasso_penalty,
        DEFAULT_LASSO_PENALTY
      )

      i <- 1
      for (layer in layers) {
        layer <- get_default_layer_params(layer)
        layer_fields <- names(layer)

        for (field in layer_fields) {
          self$fit_params[[sprintf("%s_%s", field, i)]] <- layer[[field]]
        }
        i <- i + 1
      }

      self$fit_params$optimizer <- tolower(optimizer)

      if (self$is_multivariate && !is.null(loss_function)) {
        warning(
          "In multivariate models no custom loss function can be used, the ",
          "default function will be used."
        )
        loss_function <- NULL
      }
      if (!is.null(loss_function)) {
        self$fit_params$loss_function <- tolower(loss_function)
      }

      self$fit_params$with_platt_scaling <- with_platt_scaling
      self$fit_params$platt_proportion <- platt_proportion
      self$fit_params$shuffle <- shuffle
      self$fit_params$early_stop <- early_stop
      self$fit_params$early_stop_patience <- early_stop_patience

      self$fit_params$hidden_layers_number <- length(layers)
    },
    predict = function(x) {
      x <- private$get_x_for_model(x, remove_cols = FALSE)
      if (!is.null(self$removed_x_cols)) {
        x <- x[, -self$removed_x_cols]
      }

      predict_function <- private$predict_univariate
      if (self$is_multivariate) {
        predict_function <- private$predict_multivariate
      } else if (self$fit_params$with_platt_scaling) {
        predict_function <- private$predict_platt_univariate
      }

      py_hush(predict_function(
        model = self$fitted_model,
        x = x,
        responses = self$responses,
        fit_params = self$fit_params
      ))
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    has_to_tune = function() {
      responses <- self$fit_params$responses
      y_colnames <- self$fit_params$y_colnames

      self$fit_params$responses <- NULL
      self$fit_params$y_colnames <- NULL

      result <- super$has_to_tune()

      self$fit_params$responses <- responses
      self$fit_params$y_colnames <- y_colnames

      return(result)
    },
    get_hyperparams = function() {
      hyperparams <- super$get_hyperparams()
      hyperparams$responses <- NULL
      hyperparams$y_colnames <- NULL

      return(hyperparams)
    },

    can_be_used_platt = function() {
      return(
        !self$is_multivariate &&
        !is_categorical_response(self$responses$y$type)
      )
    },

    prepare_univariate_y = function() {
      super$prepare_univariate_y()

      self$y <- prepare_y_to_deep_learning(self$y, self$responses$y$type)

      if (is_categorical_response(self$responses$y$type)) {
        colnames(self$y) <- self$responses$y$levels
      }
    },
    prepare_multivariate_y = function() {
      super$prepare_multivariate_y()

      new_y <- matrix(, nrow = nrow(self$y), ncol = 0)
      for (name in colnames(self$y)) {
        y <- prepare_y_to_deep_learning(
          self$y[[name]],
          self$responses[[name]]$type
        )

        self$responses[[name]]$colnames <- name
        if (is_categorical_response(self$responses[[name]]$type)) {
          self$responses[[name]]$colnames <- paste0(
            name,
            ".",
            self$responses[[name]]$levels
          )
        } else {
          y <- to_matrix(y)
        }

        colnames(y) <- self$responses[[name]]$colnames
        new_y <- cbind(new_y, y)
      }

      self$y <- new_y
    },
    prepare_others = function() {
      if (is_bayesian_tuner(self$tuner_class)) {
        self$fit_params$epochs_number <- format_bayes_hyperparam(
          self$fit_params$epochs_number,
          is_int = TRUE
        )
        self$fit_params$batch_size <- format_bayes_hyperparam(
          self$fit_params$batch_size,
          is_int = TRUE
        )
        self$fit_params$learning_rate <- format_bayes_hyperparam(
          self$fit_params$learning_rate
        )
      }

      # Convert all the neurons proportion to integer values
      for (i in 1:self$fit_params$hidden_layers_number) {
        neurons_i <- sprintf("neurons_number_%s", i)
        proportion_i <- sprintf("neurons_proportion_%s", i)

        layer_neurons <- self$fit_params[[neurons_i]]
        layer_proportions <- self$fit_params[[neurons_i]]

        if (is_bayesian_tuner(self$tuner_class)) {
          layer_neurons <- format_bayes_hyperparam(layer_neurons, is_int = TRUE)
          self$fit_params[[sprintf("dropout_%s", i)]] <-
            format_bayes_hyperparam(
              self$fit_params[[sprintf("dropout_%s", i)]]
            )
          self$fit_params[[sprintf("ridge_penalty_%s", i)]] <-
            format_bayes_hyperparam(
              self$fit_params[[sprintf("ridge_penalty_%s", i)]]
            )
          self$fit_params[[sprintf("lasso_penalty_%s", i)]] <-
            format_bayes_hyperparam(
              self$fit_params[[sprintf("lasso_penalty_%s", i)]]
            )

        } else {
          if (!is.null(self$fit_params[[proportion_i]])) {
            layer_proportions <- sapply(
              self$fit_params[[proportion_i]],
              proportion_to,
              to = ncol(self$x),
              upper = Inf
            )
            layer_neurons <- c(layer_neurons, layer_proportions)
          }

          layer_neurons <- ceiling(layer_neurons)
        }

        self$fit_params[[proportion_i]] <- NULL
        self$fit_params[[neurons_i]] <- layer_neurons

        activation_i <- sprintf("activation_%s", i)
        self$fit_params[[activation_i]] <- tolower(
          self$fit_params[[activation_i]]
        )
      }

      for (name in names(self$responses)) {
        self$responses[[name]]$last_layer_activation <-
          get_last_layer_activation(self$responses[[name]]$type)

        self$responses[[name]]$last_layer_neurons <-
          get_last_layer_neurons_number(
            response_type = self$responses[[name]]$type,
            levels = self$responses[[name]]$levels
          )

        self$responses[[name]]$loss_function <- nonull(
          self$fit_params$loss_function,
          get_loss(self$responses[[name]]$type)
        )

        self$responses[[name]]$metric <- get_metric(self$responses[[name]]$type)
      }

      self$fit_params$responses <- self$responses
      if (self$is_multivariate) {
        self$fit_params$y_colnames <- colnames(self$y)
      }

      if (
        !private$can_be_used_platt() &&
        self$fit_params$with_platt_scaling
      ) {
        self$fit_params$with_platt_scaling <- FALSE
        warning(
          "Platt scaling is not going to be used because it is only ",
          "available for univariate models with a numeric or binary response ",
          "variable."
        )
      }

      self$fit_params$optimizer_function <- get_keras_optimizer_function(
        self$fit_params$optimizer
      )
    },

    train = function(...) {
      if (self$is_multivariate) {
        private$train_multivariate(...)
      } else {
        if (self$fit_params$with_platt_scaling) {
          private$train_univariate_platt(...)
        } else {
          private$train_univariate(...)
        }
      }
    },

    train_univariate_platt = function(x,
                                      y,
                                      fit_params) {
      platt_indices <- sample(
        nrow(x),
        ceiling(nrow(x) * fit_params$platt_proportion)
      )

      model <- private$train_univariate(
        x = get_records(x, -platt_indices),
        y = get_records(y, -platt_indices),
        fit_params = fit_params
      )

      predictions <- private$predict_univariate(
        model = model,
        x = get_records(x, platt_indices),
        responses = self$responses,
        fit_params = fit_params
      )

      if (is_binary_response(self$responses$y$type)) {
        predictions <- predictions$probabilities[[self$responses$y$levels[2]]]
      } else {
        predictions <- predictions$predicted
      }

      platt_data <- data.frame(
        observed = get_records(y, platt_indices),
        predicted = predictions
      )

      family_name <- get_glmnet_family(
        response_type = self$responses$y$type,
        is_multivariate = FALSE
      )
      self$linear_model <- glm(
        observed ~ predicted,
        data = platt_data,
        family = family_name
      )

      return(model)
    },
    # This function is the one used for tuning
    train_univariate = function(x,
                                y,
                                fit_params,
                                x_testing = NULL,
                                y_testing = NULL) {
      responses <- fit_params$responses
      model <- keras_model_sequential()

      for (i in 1:fit_params$hidden_layers_number) {
        model <- model %>%
          layer_dense(
            units = fit_params[[sprintf("neurons_number_%s", i)]],
            activation = fit_params[[sprintf("activation_%s", i)]],
            kernel_regularizer = regularizer_l1_l2(
              l1 = fit_params[[sprintf("lasso_penalty_%s", i)]],
              l2 = fit_params[[sprintf("ridge_penalty_%s", i)]]
            ),
            input_shape = if (i == 1) ncol(x) else NULL,
            name = sprintf("hidden_layer_%s", i)
          ) %>%
          layer_dropout(rate = fit_params[[sprintf("dropout_%s", i)]])
      }

      model %>%
        layer_dense(
          units = responses$y$last_layer_neurons,
          activation = responses$y$last_layer_activation,
          kernel_regularizer = regularizer_l1_l2(
            l1 = fit_params$output_lasso_penalty,
            l2 = fit_params$output_ridge_penalty
          ),
          name = "output_layer"
        )


      model %>%
        compile(
          loss = responses$y$loss_function,
          optimizer = fit_params$optimizer_function(
            learning_rate = fit_params$learning_rate
          ),
          metrics = responses$y$metric
        )

      callbacks <- NULL
      if (fit_params$early_stop) {
        callbacks <- callback_early_stopping(
          monitor = "val_loss",
          mode = "min",
          patience = fit_params$early_stop_patience,
          verbose = 0
        )
      }

      validation_data <- NULL
      if (!is.null(x_testing) && !is.null(y_testing)) {
        validation_data <- list(x_testing, y_testing)
      }

      fit_model <- model %>%
        fit(
          x = x,
          y = y,
          shuffle = fit_params$shuffle,
          epochs = fit_params$epochs_number,
          batch_size = fit_params$batch_size,
          validation_data = validation_data,
          verbose = 0,
          callbacks = callbacks
        )

      # Add the validation loss to the returned model (for tuning)
      model$validation_loss <- tail(fit_model$metrics$val_loss, 1)

      return(model)
    },
    # This function is the one used for tuning
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  fit_params) {
      if (is_class_response(responses$y$type)) {
        probabilities <- predict(model, x, batch_size = fit_params$batch_size)
        probabilities <- format_tensorflow_probabilities(
          probabilities = probabilities,
          response_type = responses$y$type,
          levels = responses$y$levels
        )

        if (is_binary_response(responses$y$type)) {
          names(responses$y$thresholds) <- responses$y$levels
        }

        predictions <- predict_class(probabilities, responses$y)
      } else {
        predictions <- predict_numeric(predict(model, x))
      }

      return(predictions)
    },
    predict_platt_univariate = function(model,
                                        x,
                                        responses,
                                        fit_params) {
      predictions <- private$predict_univariate(
        model = model,
        x = x,
        responses = responses,
        fit_params = fit_params
      )

      if (is_binary_response(self$responses$y$type)) {
        second_level <- self$responses$y$levels[2]
        predicted <- predictions$probabilities[[second_level]]
      } else {
        predicted <- predictions$predicted
      }

      data <- data.frame(predicted = predicted)
      platt_predictions <- predict_univariate_glm(
        model = self$linear_model,
        data = data,
        response = responses$y
      )

      names(platt_predictions) <- paste0(names(platt_predictions), "_platt")

      predictions <- append(predictions, platt_predictions)

      return(predictions)
    },

    train_multivariate = function(x,
                                  y,
                                  fit_params,
                                  x_testing = NULL,
                                  y_testing = NULL) {
      responses <- fit_params$responses
      input <- layer_input(shape = ncol(x), name = "input_layer")
      base_model <- input

      for (i in 1:fit_params$hidden_layers_number) {
        base_model <- base_model %>%
          layer_dense(
            units = fit_params[[sprintf("neurons_number_%s", i)]],
            activation = fit_params[[sprintf("activation_%s", i)]],
            kernel_regularizer = regularizer_l1_l2(
              l1 = fit_params[[sprintf("lasso_penalty_%s", i)]],
              l2 = fit_params[[sprintf("ridge_penalty_%s", i)]]
            ),
            name = sprintf("hidden_layer_%s", i)
          ) %>%
          layer_dropout(rate = fit_params[[sprintf("dropout_%s", i)]])
      }

      output_layers <- list(
        layers = list(),
        losses = list(),
        metrics = list(),
        y = list()
      )

      for (name in names(responses)) {
        layer <- base_model %>%
          layer_dense(
            units = responses[[name]]$last_layer_neurons,
            activation = responses[[name]]$last_layer_activation,
            kernel_regularizer = regularizer_l1_l2(
              l1 = fit_params$output_lasso_penalty,
              l2 = fit_params$output_ridge_penalty
            ),
            name = name
          )

        output_layers$layers[[name]] <- layer
        output_layers$losses[[name]] <- responses[[name]]$loss_function
        output_layers$metrics[[name]] <- responses[[name]]$metric
        output_layers$y[[name]] <- y[, responses[[name]]$colnames]
      }

      model <- keras_model(input, output_layers$layers) %>%
        compile(
          optimizer = fit_params$optimizer_function(
            learning_rate = fit_params$learning_rate
          ),
          loss = output_layers$losses,
          metrics = output_layers$metrics,
          loss_weights = rep(1, length(responses))
        )

      callbacks <- NULL
      if (fit_params$early_stop) {
        callbacks <- callback_early_stopping(
          monitor = "val_loss",
          mode = "min",
          patience = fit_params$early_stop_patience,
          verbose = 0
        )
      }

      validation_data <- NULL
      if (!is.null(x_testing) && !is.null(y_testing)) {
        validation_data <- list(x = x_testing)
        y_validation <- list()

        for (name in names(responses)) {
          y_validation[[name]] <- y_testing[, responses[[name]]$colnames]
        }

        validation_data$y <- y_validation
      }

      fit_model <- model %>%
        fit(
          x = x,
          y = output_layers$y,
          shuffle = fit_params$shuffle,
          epochs = fit_params$epochs_number,
          batch_size = fit_params$batch_size,
          validation_data = validation_data,
          verbose = 0,
          callbacks = callbacks
        )

      # Add the validation loss to the returned model (for tuning)
      model$validation_loss <- tail(fit_model$metrics$val_loss, 1)

      return(model)
    },
    predict_multivariate = function(model,
                                    x,
                                    responses,
                                    fit_params) {
      predictions <- list()
      all_predictions <- data.frame(predict(
        model,
        x,
        batch_size = fit_params$batch_size
      ))
      colnames(all_predictions) <- fit_params$y_colnames

      for (name in names(responses)) {
        cols_names <- responses[[name]]$colnames

        if (is_class_response(responses[[name]]$type)) {
          probabilities <- all_predictions[, cols_names]

          probabilities <- format_tensorflow_probabilities(
            probabilities = probabilities,
            response_type = responses[[name]]$type,
            levels = responses[[name]]$levels
          )

          predictions[[name]] <- predict_class(probabilities, responses[[name]])
        } else {
          predictions[[name]] <- predict_numeric(all_predictions[, cols_names])
        }
      }

      return(predictions)
    }
  )
)
