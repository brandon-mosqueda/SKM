#' @import dplyr

#' @include utils.R

prepare_univariate_y <- function() {
  if (is.data.frame(self$y)) {
    self$y <- unlist(self$y, use.names = FALSE)
  } else if (is.matrix(self$y)) {
    self$y <- c(self$y)
  }

  if (is.character(self$y)) {
    self$y <- factor(self$y)
  }

  self$responses$y <- list(
    type = get_response_type(self$y),
    levels = levels(self$y)
  )
}

prepare_multivariate_y <- function() {
  if (is.matrix(self$y)) {
    self$y <- as.data.frame(self$y)
  }

  self$y <- self$y %>%
    mutate_if(is.character, factor) %>%
    mutate_if(is.logical, factor)

  for (col_name in colnames(self$y)) {
    self$responses[[col_name]] <- list(
      type = get_response_type(self$y[[col_name]]),
      levels = levels(self$y[[col_name]])
    )
  }
}

prepare_degree <- function(kernel, degree) {
  if (is.null(kernel) || !(tolower(kernel) %in% tolower(KERNELS_WITH_DEGREE))) {
    return(NULL)
  }

  return(degree)
}

prepare_gamma <- function(kernel, gamma) {
  if (is.null(kernel) || !(tolower(kernel) %in% tolower(KERNELS_WITH_GAMMA))) {
    return(NULL)
  }

  return(gamma)
}

prepare_coef0 <- function(kernel, coef0) {
  if (is.null(kernel) || !(tolower(kernel) %in% tolower(KERNELS_WITH_COEF0))) {
    return(NULL)
  }

  return(coef0)
}

get_cross_validator <- function(type,
                                records_number,
                                folds_number,
                                testing_proportion) {
  type <- tolower(type)

  validator <- NULL
  if (type == "k_fold") {
    validator <- KFoldCV
  } else if (type == "random") {
    validator <- RandomCV
  } else {
    stop(sprintf(
      "{%s} is not a valid type of cross validation",
      set_collapse(type)
    ))
  }

  instance <- validator$new(
    folds_number = folds_number,
    records_number = records_number,
    testing_proportion = testing_proportion
  )

  return(instance)
}

proportion_to <- function(proportion, to, lower = 0, upper = 1) {
  if (is.null(proportion)) {
    return(NULL)
  }

  return(ifelse(
    proportion >= lower & proportion <= upper,
    ceiling(proportion * to),
    proportion
  ))
}

get_glmnet_family <- function(response_type, is_multivariate) {
  if (is_multivariate) {
    family <- "mgaussian"
  } else if (is_continuous_response(response_type)) {
    family <- "gaussian"
  } else if (is_discrete_response(response_type)) {
    family <- "poisson"
  } else if (is_categorical_response(response_type)) {
    family <- "multinomial"
  } else if (is_binary_response(response_type)) {
    family <- "binomial"
  }

  return(family)
}

train_glm <- function(x, y, hyperparams, other_params) {
  model <- glmnet(
    x = x,
    y = y,

    family = other_params$response_family,

    alpha = hyperparams$alpha,
    lambda = hyperparams$lambda,

    nlambda = other_params$lambdas_number,
    lambda.min.ratio = other_params$lambda_min_ratio,
    weights = other_params$records_weights,
    standardize = other_params$standardize,
    intercept = other_params$fit_intercept
  )

  return(model)
}

prepare_random_forest_na_action <- function(na_action) {
  na_action <- tolower(na_action)

  if (na_action == "omit") {
    na_action <- "na.omit"
  } else if (na_action == "impute") {
    na_action <- "na.impute"
  } else {
    stop(na_action, "is not a valid random forest na action")
  }

  return(na_action)
}

get_random_forest_formula <- function(responses,
                                      is_multivariate,
                                      is_regression_model) {
  if (is_multivariate) {
    responses_comma <- paste0(names(responses), collapse = ", ")

    model_formula <- sprintf("Multivar(%s) ~ .", responses_comma)
    if (is_regression_model) {
      model_formula <- sprintf("cbind(%s) ~ .", responses_comma)
    }
  } else {
    model_formula <- "y ~ ."
  }

  return(formula(model_formula))
}

train_random_forest <- function(x, y, hyperparams, other_params) {
  data <- data.frame(y, x)

  model <- rfsrc(
    formula = other_params$model_formula,
    data = data,
    ntree = hyperparams$trees_number,
    mtry = hyperparams$sampled_x_vars_number,
    nodesize = hyperparams$node_size,
    nodedepth = hyperparams$node_depth,

    importance = other_params$importance,
    splitrule = other_params$split_rule,
    nsplit = other_params$splits_number,
    xvar.wt = other_params$x_vars_weights,
    case.wt = other_params$records_weights,
    na.action = other_params$na_action
  )

  return(model)
}

get_gbm_distribution <- function(response_type) {
  if (is_continuous_response(response_type)) {
    distribution <- "gaussian"
  } else if (is_binary_response(response_type)) {
    distribution <- "bernoulli"
  } else if (is_categorical_response(response_type)) {
    distribution <- "multinomial"
  } else if (is_discrete_response(response_type)) {
    distribution <- "poisson"
  } else {
    stop(sprintf(
      "{%s} is not a valid type of response",
      set_collapse(response_type)
    ))
  }

  return(distribution)
}

get_gbm_predict_type <- function(response_type) {
  if (is_numeric_response(response_type)) {
    type <- "link"
  } else if (is_class_response(response_type)) {
    type <- "response"
  } else {
    stop(sprintf(
      "{%s} is not a valid type of response",
      set_collapse(response_type)
    ))
  }

  return(type)
}

remove_if_has_more <- function(x, compare_value, indices_to_remove) {
  if (
    !is.null(indices_to_remove) &&
    !is_empty(x) &&
    get_length(x) > compare_value
  ) {
    x <- get_records(x, -indices_to_remove)
  }

  return(x)
}

# Deep learning --------------------------------------------------

get_default_layer_params <- function(layer) {
  layer$neurons_number <- nonull(layer$neurons_number, DEFAULT_LAYER_NEURONS)
  layer$activation <- nonull(layer$activation, DEFAULT_LAYER_ACTIVATION)
  layer$dropout <- nonull(layer$dropout, DEFAULT_LAYER_DROPOUT)
  layer$ridge_penalty <- nonull(layer$ridge_penalty, DEFAULT_RIDGE_PENALTY)
  layer$lasso_penalty <- nonull(layer$lasso_penalty, DEFAULT_LASSO_PENALTY)

  return(layer)
}

get_last_layer_activation <- function(response_type) {
  if (is_continuous_response(response_type)) {
    activation <- "linear"
  } else if(is_discrete_response(response_type)) {
    activation <- "exponential"
  } else if (is_binary_response(response_type)) {
    activation <- "sigmoid"
  } else if (is_categorical_response(response_type)) {
    activation <- "softmax"
  } else {
    stop(sprintf(
      "{%s} is not a valid type of response",
      set_collapse(response_type)
    ))
  }

  return(activation)
}

get_last_layer_neurons_number <- function(response_type, levels) {
  units <- 1

  if (is_categorical_response(response_type)) {
    units <- length(levels)
  }

  return(units)
}

get_loss <- function(response_type) {
  if (is_continuous_response(response_type)) {
    loss <- "mse"
  } else if (is_discrete_response(response_type)) {
    loss <- "poisson"
  } else if (is_categorical_response(response_type)) {
    loss <- "categorical_crossentropy"
  } else if (is_binary_response(response_type)) {
    loss <- "binary_crossentropy"
  } else {
    stop(sprintf(
      "{%s} is not a valid type of response",
      set_collapse(response_type)
    ))
  }

  return(loss)
}

get_metric <- function(response_type) {
  if (is_numeric_response(response_type)) {
    metric <- "mse"
  } else if (is_class_response(response_type)) {
    metric <- "accuracy"
  } else {
    stop(sprintf(
      "{%s} is not a valid type of response",
      set_collapse(response_type)
    ))
  }

  return(metric)
}

prepare_y_to_deep_learning <- function(y, response_type) {
  if (is_categorical_response(response_type)) {
    y <- to_categorical(as.numeric(y) - 1)
  } else if (is_binary_response(response_type)) {
    y <- as.numeric(y) - 1
  }

  return(y)
}

predict_class <- function(probabilities, response_type, levels) {
  if (is_binary_response(response_type)) {
    # With binary responses, probabilities is a vector that refers to level 2
    predictions <- ifelse(probabilities > 0.5, 2, 1)
    predictions <- levels[predictions]

    probabilities <- cbind(1 - probabilities, probabilities)
  } else if (is_categorical_response(response_type)) {
    predictions <- apply(probabilities, 1, which.max)
    predictions <- levels[predictions]
  } else {
    stop(sprintf(
      "{%s} is not a class response type",
      set_collapse(response_type)
    ))
  }

  predictions <- factor(predictions, levels = levels)
  colnames(probabilities) <- levels

  return(list(
    predicted = predictions,
    probabilities = probabilities
  ))
}

predict_numeric <- function(predictions) {
  return(list(
    predicted = as.numeric(predictions)
  ))
}
