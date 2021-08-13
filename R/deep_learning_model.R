#' @importFrom R6 R6Class

#' @import keras

#' @include utils.R
#' @include model.R
#' @include globals.R
#' @include deep_learning_tuner.R
#' @include model_helpers.R

DeepLearningModel <- R6Class(
  classname = "DeepLearningModel",
  inherit = Model,
  public = list(
    # Properties --------------------------------------------------

    linear_model = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...,
                          learning_rate,
                          epochs_number,
                          batch_size,
                          layers,
                          output_penalties,

                          with_platt_scaling,
                          platt_proportion,
                          shuffle,
                          early_stop,
                          early_stop_patience) {
      super$initialize(..., name = "Deep Learning")

      self$tuner_class <- DeepLearningTuner

      self$hyperparams$learning_rate <- learning_rate
      self$hyperparams$epochs_number <- epochs_number
      self$hyperparams$batch_size <- batch_size
      self$hyperparams$output_ridge_penalty <- nonull(
        output_penalties$ridge_penalty,
        DEFAULT_RIDGE_PENALTY
      )
      self$hyperparams$output_lasso_penalty <- nonull(
        output_penalties$lasso_penalty,
        DEFAULT_LASSO_PENALTY
      )

      i <- 1
      for (layer in layers) {
        layer <- get_default_layer_params(layer)
        layer_fields <- names(layer)

        for (field in layer_fields) {
          self$hyperparams[[sprintf("%s_%s", field, i)]] <- layer[[field]]
        }
        i <- i + 1
      }

      self$other_params$with_platt_scaling <- with_platt_scaling
      self$other_params$platt_proportion <- platt_proportion
      self$other_params$shuffle <- shuffle
      self$other_params$early_stop <- early_stop
      self$other_params$early_stop_patience <- early_stop_patience

      self$other_params$hidden_layers_number <- length(layers)
    },
    predict = function(x) {
      x <- private$get_x_for_model(x, remove_cols = FALSE)
      if (!is.null(self$removed_x_cols)) {
        x <- x[, -self$removed_x_cols]
      }

      predict_function <- private$predict_univariate
      if (self$is_multivariate) {
        predict_function <- private$predict_multivariate
      } else if (self$other_params$with_platt_scaling) {
        predict_function <- private$predict_platt_univariate
      }

      py_hush(predict_function(
        model = self$fitted_model,
        x = x,
        responses = self$responses,
        other_params = self$other_params,
        hyperparams = self$hyperparams
      ))
    }
  ),
  private = list(
    # Methods --------------------------------------------------

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
      # Convert all the neurons proportion to integer values
      for (i in 1:self$other_params$hidden_layers_number) {
        neurons_i <- sprintf("neurons_number_%s", i)
        proportion_i <- sprintf("neurons_proportion_%s", i)

        layer_neurons <- self$hyperparams[[neurons_i]]
        if (!is.null(self$hyperparams[[proportion_i]])) {
          layer_proportions <- sapply(
            self$hyperparams[[proportion_i]],
            proportion_to,
            to = ncol(self$x),
            upper = Inf
          )
          layer_neurons <- c(layer_neurons, layer_proportions)
        }
        self$hyperparams[[proportion_i]] <- NULL
        layer_neurons <- ceiling(layer_neurons)
        self$hyperparams[[neurons_i]] <- layer_neurons

        activation_i <- sprintf("activation_%s", i)
        self$hyperparams[[activation_i]] <- tolower(
          self$hyperparams[[activation_i]]
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

        self$responses[[name]]$loss_function <- get_loss(
          self$responses[[name]]$type
        )

        self$responses[[name]]$metric <- get_metric(self$responses[[name]]$type)
      }

      self$other_params$responses <- self$responses
      if (self$is_multivariate) {
        self$other_params$y_colnames <- colnames(self$y)
      }

      if (
        !private$can_be_used_platt() &&
        self$other_params$with_platt_scaling
      ) {
        self$other_params$with_platt_scaling <- FALSE
        warning(
          "Platt scaling is not going to be used because it is only ",
          "available for univariate models with numeric or binary response ",
          "variable."
        )
      }
    },

    train = function(...) {
      if (self$is_multivariate) {
        private$train_multivariate(...)
      } else {
        if (self$other_params$with_platt_scaling) {
          private$train_univariate_platt(...)
        } else {
          private$train_univariate(...)
        }
      }
    },

    train_univariate_platt = function(x,
                                      y,
                                      hyperparams,
                                      other_params) {
      platt_indices <- sample(
        nrow(x),
        ceiling(nrow(x) * other_params$platt_proportion)
      )

      model <- private$train_univariate(
        x = get_records(x, -platt_indices),
        y = get_records(y, -platt_indices),
        hyperparams = hyperparams,
        other_params = other_params
      )

      predictions <- private$predict_univariate(
        model = model,
        x = get_records(x, platt_indices),
        responses = self$responses,
        other_params = other_params,
        hyperparams = hyperparams
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
                                hyperparams,
                                other_params,
                                x_testing = NULL,
                                y_testing = NULL) {
      responses <- other_params$responses
      model <- keras_model_sequential()

      for (i in 1:other_params$hidden_layers_number) {
        model <- model %>%
          layer_dense(
            units = hyperparams[[sprintf("neurons_number_%s", i)]],
            activation = hyperparams[[sprintf("activation_%s", i)]],
            kernel_regularizer = regularizer_l1_l2(
              l1 = hyperparams[[sprintf("lasso_penalty_%s", i)]],
              l2 = hyperparams[[sprintf("ridge_penalty_%s", i)]]
            ),
            input_shape = if (i == 1) ncol(x) else NULL,
            name = sprintf("hidden_layer_%s", i)
          ) %>%
          layer_dropout(rate = hyperparams[[sprintf("dropout_%s", i)]])
      }

      model %>%
        layer_dense(
          units = responses$y$last_layer_neurons,
          activation = responses$y$last_layer_activation,
          kernel_regularizer = regularizer_l1_l2(
            l1 = hyperparams$output_lasso_penalty,
            l2 = hyperparams$output_ridge_penalty
          ),
          name = "output_layer"
        )

      model %>%
        compile(
          loss = responses$y$loss_function,
          optimizer = optimizer_adam(lr = hyperparams$learning_rate),
          metrics = responses$y$metric
        )

      callbacks <- NULL
      if (other_params$early_stop) {
        callbacks <- callback_early_stopping(
          monitor = "val_loss",
          mode = "min",
          patience = other_params$early_stop_patience,
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
          shuffle = other_params$shuffle,
          epochs = hyperparams$epochs_number,
          batch_size = hyperparams$batch_size,
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
                                  other_params,
                                  hyperparams) {
      if (is_class_response(responses$y$type)) {
        probabilities <- predict_proba(model, x)

        predictions <- predict_class(
          probabilities = as.data.frame(probabilities),
          response_type = responses$y$type,
          levels = responses$y$levels
        )
      } else {
        predictions <- predict_numeric(predict(model, x))
      }

      return(predictions)
    },
    predict_platt_univariate = function(model,
                                        x,
                                        responses,
                                        other_params,
                                        hyperparams) {
      predictions <- private$predict_univariate(
        model = model,
        x = x,
        responses = responses,
        other_params = other_params,
        hyperparams = hyperparams
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
                                  hyperparams,
                                  other_params,
                                  x_testing = NULL,
                                  y_testing = NULL) {
      responses <- other_params$responses
      input <- layer_input(shape = ncol(x), name = "input_layer")
      base_model <- input

      for (i in 1:other_params$hidden_layers_number) {
        base_model <- base_model %>%
          layer_dense(
            units = hyperparams[[sprintf("neurons_number_%s", i)]],
            activation = hyperparams[[sprintf("activation_%s", i)]],
            kernel_regularizer = regularizer_l1_l2(
              l1 = hyperparams[[sprintf("lasso_penalty_%s", i)]],
              l2 = hyperparams[[sprintf("ridge_penalty_%s", i)]]
            ),
            name = sprintf("hidden_layer_%s", i)
          ) %>%
          layer_dropout(rate = hyperparams[[sprintf("dropout_%s", i)]])
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
              l1 = hyperparams$output_lasso_penalty,
              l2 = hyperparams$output_ridge_penalty
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
          optimizer = optimizer_adam(
            lr = hyperparams$learning_rate
          ),
          loss = output_layers$losses,
          metrics = output_layers$metrics,
          loss_weights = rep(1, length(responses))
        )

      callbacks <- NULL
      if (other_params$early_stop) {
        callbacks <- callback_early_stopping(
          monitor = "val_loss",
          mode = "min",
          patience = other_params$early_stop_patience,
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
          shuffle = other_params$shuffle,
          epochs = hyperparams$epochs_number,
          batch_size = hyperparams$batch_size,
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
                                    other_params,
                                    hyperparams) {
      predictions <- list()
      all_predictions <- data.frame(predict(model, x))
      colnames(all_predictions) <- other_params$y_colnames

      for (name in names(responses)) {
        cols_names <- responses[[name]]$colnames

        if (is_class_response(responses[[name]]$type)) {
          probabilities <- all_predictions[, cols_names]

          predictions[[name]] <- predict_class(
            probabilities = as.data.frame(probabilities),
            response_type = responses[[name]]$type,
            levels = responses[[name]]$levels
          )
        } else {
          predictions[[name]] <- predict_numeric(all_predictions[, cols_names])
        }
      }

      return(predictions)
    }
  )
)
