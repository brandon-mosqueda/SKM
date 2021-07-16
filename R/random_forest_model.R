#' @importFrom R6 R6Class
#' @importFrom randomForestSRC rfsrc

#' @include model.R

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
                          importance,
                          x_vars_weights,
                          records_weights) {
      super$initialize(..., name = "Random Forest")

      self$hyperparams$trees_number <- trees_number
      self$hyperparams$node_size <- node_size
      self$hyperparams$node_depth <- node_depth
      self$hyperparams$sampled_x_vars_number <- sampled_x_vars_number

      self$other_params$split_rule <- split_rule
      self$other_params$splits_number <- splits_number
      self$other_params$importance <- importance
      self$other_params$x_vars_weights <- x_vars_weights
      self$other_params$records_weights <- records_weights
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    prepare_others = function() {
      self$hyperparams$sampled_x_vars_number <- proportion_to(
        self$hyperparams$sampled_x_vars_number,
        ncol(self$x)
      )

      if (self$is_multivariate) {
        self$is_regression_model <- all(sapply(
          self$responses,
          function(x) is_numeric_response(x$type)
        ))
      }
    },
    get_x_for_model = function(x, remove_cols = FALSE) {
      return(to_data_frame(x))
    },

    tune = function() {
      # When tuning use importance FALSE for quicker evalution
      true_importance <- self$other_params$importance
      self$other_params$importance <- FALSE
      super$tune()
      self$other_params$importance <- true_importance
    },

    train_univariate = function(x, y, hyperparams, other_params) {
      data <- data.frame(y = y, x)

      model <- rfsrc(
        y ~ .,
        data = data,
        ntree = hyperparams$trees_number,
        mtry = hyperparams$sampled_x_vars_number,
        nodesize = hyperparams$node_size,
        nodedepth = hyperparams$node_depth,

        importance = other_params$importance,
        splitrule = other_params$split_rule,
        nsplit = other_params$splits_number,
        xvar.wt = other_params$x_vars_weights,
        case.wt = other_params$records_weights
      )

      return(model)
    },
    predict_univariate = function(model,
                                  x,
                                  responses,
                                  other_params,
                                  hyperparams) {
      x <- data.frame(x)
      predictions <- predict(model, newdata = x)

      if (is_class_response(responses[["y"]]$type)) {
        predictions <- list(
          predicted = predictions$class,
          probabilities = as.data.frame(predictions$predicted)
        )
      } else {
        predictions <- list(predicted = predictions$predicted)
      }

      return(predictions)
    },

    train_multivariate = function(x, y, hyperparams, other_params) {
      data <- data.frame(y, x)
      responses_comma <- paste0(colnames(y), collapse = ", ")

      model_formula <- sprintf("Multivar(%s) ~ .", responses_comma)
      if (self$is_regression_model) {
        model_formula <- sprintf("cbind(%s) ~ .", responses_comma)
      }
      model_formula <- formula(model_formula)

      model <- rfsrc(
        model_formula,
        data = data,
        ntree = hyperparams$trees_number,
        mtry = hyperparams$sampled_x_vars_number,
        nodesize = hyperparams$node_size,
        nodedepth = hyperparams$node_depth,

        importance = other_params$importance,
        splitrule = other_params$split_rule,
        nsplit = other_params$splits_number,
        xvar.wt = other_params$x_vars_weights,
        case.wt = other_params$records_weights
      )

      return(model)
    },
    predict_multivariate = function(model,
                                    x,
                                    responses,
                                    other_params,
                                    hyperparams) {
      x <- data.frame(x)
      all_predictions <- predict(model, newdata = x)
      predictions <- list()

      for (response_name in names(responses)) {
        response_type <- responses[[response_name]]$type

        if (is_class_response(response_type)) {
          response_predictions <- all_predictions$classOutput[[response_name]]

          predictions[[response_name]] <- list(
            predicted = response_predictions$class,
            probabilities = response_predictions$predicted
          )
        } else {
          predictions[[response_name]] <- list(
            predicted = all_predictions$regrOutput[[response_name]]$predicted
          )
        }
      }

      return(predictions)
    }
  )
)