#' @import keras
#' @import dplyr

#' @importFrom glmnet cv.glmnet

#' @include utils.R
#' @include validator.R
#' @include kernels.R
#' @include bayesian_model.R

prepare_univariate_y <- function() {
  if (is.data.frame(self$y)) {
    self$y <- unlist(self$y, use.names = FALSE)
  } else if (is.matrix(self$y)) {
    self$y <- c(self$y)
  }

  if (is.character(self$y) || is.logical(self$y)) {
    self$y <- factor(self$y)
  }

  self$responses$y <- list(
    type = get_response_type(self$y),
    levels = levels(self$y)
  )

  if (is_class_response(self$responses$y$type)) {
    self$responses$y$thresholds <- as.list(table(self$y) / length(self$y))
  }
}

prepare_multivariate_y <- function() {
  if (is.matrix(self$y)) {
    self$y <- as.data.frame(self$y)
  }

  cols_to_factor <- sapply(self$y, function(x) is.character(x) || is.logical(x))
  self$y[cols_to_factor] <- lapply(self$y[cols_to_factor], factor)

  for (col_name in colnames(self$y)) {
    self$responses[[col_name]] <- list(
      type = get_response_type(self$y[[col_name]]),
      levels = levels(self$y[[col_name]])
    )

    if (is_class_response(self$responses[[col_name]]$type)) {
      self$responses[[col_name]]$thresholds <- as.list(
        table(self$y[[col_name]]) / nrow(self$y)
      )
    }
  }
}

prepare_multivariate_y_only_numeric <- function() {
  super$prepare_multivariate_y()

  are_all_numeric <- all(sapply(
    self$responses,
    function(response) is_numeric_response(response$type)
  ))

  if (!are_all_numeric) {
    warning(
      "In ",
      self$name,
      " multivariate models it can only be used ",
      "numeric responses variables, so some of the responses were ",
      "converted to numeric"
    )

    for (name in names(self$responses)) {
      self$responses[[name]]$type <- RESPONSE_TYPES$CONTINUOUS
      self$responses[[name]]$levels <- NULL
    }
  }

  self$y <- data.matrix(self$y)
}

get_cross_validator <- function(type,
                                records_number,
                                folds_number,
                                testing_proportion,
                                folds) {
  type <- tolower(type)

  if (!is.null(folds)) {
    return(CustomCV$new(folds = folds))
  } else if (type == "k_fold") {
    return(KFoldCV$new(
      folds_number = folds_number,
      records_number = records_number
    ))
  } else if (type == "random") {
    return(RandomCV$new(
      folds_number = folds_number,
      records_number = records_number,
      testing_proportion = testing_proportion
    ))
  } else {
    stop(sprintf(
      "{%s} is not a valid type of cross validation",
      set_collapse(type)
    ))
  }
}

get_tuner <- function(type) {
  type <- tolower(type)

  if (type == "grid_search") {
    tuner <- GridTuner
  } else if (type == "bayesian_optimization") {
    tuner <- BayesianTuner
  } else if (type == "deep_grid_search") {
    tuner <- DeepLearningGridTuner
  } else if (type == "deep_bayesian_optimization") {
    tuner <- DeepLearningBayesianTuner
  }  else if (type == "glm_grid_search") {
    tuner <- GLMGridTuner
  } else if (type == "glm_bayesian_optimization") {
    tuner <- GLMBayesianTuner
  } else if (type == "radial_grid_search") {
    tuner <- RadialGridTuner
  } else if (type == "radial_bayesian_optimization") {
    tuner <- RadialBayesianTuner
  } else {
    stop(sprintf(
      "{%s} is not a valid type of tuning",
      set_collapse(type)
    ))
  }

  return(tuner)
}

is_bayesian_tuner <- function(tuner) {
  if (is.character(tuner)) {
    return(tolower(tuner) == "bayesian_optimization")
  }

  return(
    tuner$classname == "BayesianTuner" ||
    tuner$classname == "DeepLearningBayesianTuner"
  )
}

has_to_tune <- function() {
  for (hyperparam in self$fit_params) {
    if (is_hyperparam(hyperparam)) {
      return(TRUE)
    }
  }

  return(FALSE)
}

format_bayes_hyperparam <- function(hyperparam, is_int = FALSE) {
  if (!is.list(hyperparam)) return(hyperparam)

  hyperparam <- c(hyperparam$min, hyperparam$max)

  return(if (is_int) as.integer(hyperparam) else hyperparam)
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

is_hyperparam <- function(x) {
  return((is.list(x) || length(x) > 1) && !inherits(x, "formula"))
}

format_predictions <- function(predictions, is_multivariate, format) {
  assert_predict_format(format)

  if (format == "data.frame") {
    if (is_multivariate) {
      predictions <- as.data.frame(lapply(predictions, function(x) x$predicted))
    }

    predictions <- as.data.frame(predictions)
    names(predictions) <- replace_by_regex(
      names(predictions),
      "",
      "probabilities\\."
    )
  }

  return(predictions)
}

predict_class <- function(probabilities, response_info) {
  levels <- response_info$levels

  if (is_binary_response(response_info$type)) {
    first_class <- levels[1]
    second_class <- levels[2]

    predicted <- ifelse(
      # The class with less elements has the higher probability
      probabilities[[first_class]] >= response_info$thresholds[[first_class]],
      first_class,
      second_class
    )
    classification_probabilities <- probabilities
  } else {
    thresholds <- unlist(response_info$thresholds)
    thresholds <- thresholds[colnames(probabilities)]
    classification_probabilities <- apply(probabilities, 1, function(x) {
      b <- thresholds / (1 - thresholds)

      return(x / (x + (b * (1 - x))))
    })
    classification_probabilities <- t(classification_probabilities)

    predicted <- apply(classification_probabilities, 1, which.max)
    predicted <- levels[predicted]
  }

  return(list(
    predicted = factor(predicted, levels = levels),
    probabilities = probabilities,
    thresholds = response_info$thresholds,
    classification_probabilities = classification_probabilities
  ))
}

# GBM --------------------------------------------------

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

# Random Forest --------------------------------------------------

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

train_random_forest <- function(x, y, fit_params) {
  # In this format for multivariate analysis
  data <- data.frame(y, x, check.names = FALSE)

  model <- rfsrc(
    formula = fit_params$model_formula,
    data = data,
    ntree = fit_params$trees_number,
    mtry = fit_params$sampled_x_vars_number,
    nodesize = fit_params$node_size,
    nodedepth = fit_params$node_depth,

    splitrule = fit_params$split_rule,
    nsplit = fit_params$splits_number,
    xvar.wt = fit_params$x_vars_weights,
    case.wt = fit_params$records_weights,
    na.action = fit_params$na_action,
    importance = TRUE
  )

  return(model)
}

# glm --------------------------------------------------

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

get_glmnet_loss <- function(response_type, is_multivariate) {
  if (is_multivariate || is_numeric_response(response_type)) {
    loss <- "mse"
  } else {
    loss <- "class"
  }

  return(loss)
}

train_glm <- function(x, y, fit_params) {
  model <- cv.glmnet(
    x = x,
    y = y,

    family = fit_params$response_family,
    type.measure = fit_params$cv_loss,

    alpha = fit_params$alpha,

    foldid = fit_params$folds,
    nfolds = fit_params$cv_folds_number,

    nlambda = fit_params$lambdas_number,
    weights = fit_params$records_weights,
    standardize = fit_params$standardize,
    intercept = fit_params$fit_intercept
  )

  return(model)
}

predict_univariate_glm <- function(model, data, response) {
  predictions <- predict(model, newdata = data)

  if (is_numeric_response(response$type)) {
    names(predictions) <- NULL
    predictions <- list(predicted = predictions)
  } else if (is_binary_response(response$type)) {
    probabilities <- cbind(1 - predictions, predictions)
    colnames(probabilities) <- response$levels

    predictions <- ifelse(predictions > 0.5, 2, 1)
    predictions <- response$levels[predictions]

    predictions <- list(
      predicted = factor(predictions, levels = response$levels),
      probabilities = as.data.frame(probabilities)
    )
  } else {
    stop("Not implement for other types of response variables")
  }

  return(predictions)
}

format_glmnet_folds <- function(folds) {
  records_number <- length(folds[[1]]$training) + length(folds[[1]]$testing)
  new_folds <- rep(1, records_number)

  for (i in seq_along(folds)) {
    new_folds[folds[[i]]$testing] <- i
  }

  return(new_folds)
}

# SVM --------------------------------------------------

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

# Deep learning --------------------------------------------------

deep_learning_eval_one_fold <- function(fold, combination) {
  hyperparams <- replace_at_list(self$fit_params, combination)

  x_training <- get_records(self$x, fold$training)
  y_training <- get_records(self$y, fold$training)
  x_testing <- get_records(self$x, fold$testing)
  y_testing <- get_records(self$y, fold$testing)

  model <- self$training_function(
    x = x_training,
    y = y_training,
    fit_params = hyperparams,
    x_testing = x_testing,
    y_testing = y_testing
  )

  keras::k_clear_session()
  tensorflow::tf$compat$v1$keras$backend$get_session()$close()

  return(model$validation_loss)
}

deep_learning_tune <- function() {
  super$tune()

  n_cols <- ncol(self$all_combinations)
  new_loss <- ifelse(
    self$is_multivariate,
    "loss",
    self$responses$y$loss_function
  )

  colnames(self$all_combinations)[n_cols] <- new_loss
  names(self$best_combination)[n_cols] <- new_loss

  return(invisible(self$best_combination))
}

get_keras_optimizer_function <- function(optimizer) {
  return(switch(
    tolower(optimizer),

    adadelta = optimizer_adadelta,
    adagrad = optimizer_adagrad,
    adamax = optimizer_adamax,
    adam = optimizer_adam,
    nadam = optimizer_nadam,
    rmsprop = optimizer_rmsprop,
    sgd = optimizer_sgd,

    stop("Invalid optimizer")
  ))
}

get_default_layer_params <- function(layer) {
  if (is.null(layer$neurons_number) && is.null(layer$neurons_proportion)) {
    layer$neurons_number <- DEFAULT_LAYER_NEURONS
  }
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
    loss <- "mean_squared_error"
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

format_tensorflow_probabilities <- function(probabilities,
                                            response_type,
                                            levels) {
  probabilities <- as.data.frame(probabilities)

  if (is_binary_response(response_type)) {
    probabilities <- cbind(1 - probabilities, probabilities)
  }

  colnames(probabilities) <- levels

  return(probabilities)
}

predict_numeric <- function(predictions) {
  return(list(
    predicted = as.numeric(predictions)
  ))
}

# Bayesian --------------------------------------------------

get_bglr_response_type <- function(response_type) {
  if (is_class_response(response_type)) {
    return("ordinal")
  } else if (is_numeric_response(response_type)) {
    return("gaussian")
  } else {
    stop(sprintf(
      "{%s} is not a valid response type",
      set_collapse(response_type)
    ))
  }
}

prepare_bayesian_model <- function(model) {
  if (is.null(model)) {
    return("BRR")
  }

  return(switch(
    tolower(model),

    fixed = "FIXED",
    bgblup = "RKHS",
    brr = "BRR",
    bayes_lasso = "BL",
    bayes_a = "BayesA",
    bayes_b = "BayesB",
    bayes_c = "BayesC",

    stop(sprintf("{%s} is not a valid bayesian model", model))
  ))
}

prepare_covariance_type <- function(type) {
  lower_type <- tolower(type)

  return(switch(
    lower_type,
    unstructured = "UN",
    diagonal = "DIAG",
    factor_analytic = "FA",
    recursive = "REC",
    stop(sprintf("{%s} is not a valid covariance structure type", type))
  ))
}

get_bglr_matrix_param_name <- function(model) {
  if (tolower(model) == "rkhs") {
    return("K")
  }

  return("X")
}

# Partial Least Squares --------------------------------------------------

prepare_partial_least_squares_method <- function(method) {
  method <- tolower(method)

  if (method == "kernel") {
    method <- "kernelpls"
  } else if (method == "wide_kernel") {
    method <- "widekernelpls"
  } else if (method == "simpls") {
    method <- "simpls"
  } else if (method == "orthogonal") {
    method <- "oscorespls"
  } else {
    stop(method, "is not a valid partial least squares method")
  }

  return(method)
}

get_partial_least_squares_formula <- function(responses, is_multivariate) {
  model_formula <- "y ~ ."
  if (is_multivariate) {
    responses_comma <- paste0(names(responses), collapse = ", ")
    model_formula <- sprintf("cbind(%s) ~ .", responses_comma)
  }

  return(formula(model_formula))
}

# GS radial --------------------------------------------------

format_predictors <- function(predictors) {
  names(predictors) <- tolower(names(predictors))

  for (predictor in names(predictors)) {
    predictors[[predictor]] <- tolower(predictors[[predictor]])
  }

  return(predictors)
}

prepare_eta <- function(Pheno, geno_preparator, rho, predictors, raise_to_rho) {
  eta <- list()

  # Line predictors is always included
  Line <- model.matrix(~ 0 + Line, Pheno)

  kernel_rho <- exp(1)
  if (!raise_to_rho) {
    kernel_rho <- rho
  }
  GenoKernel <- radial_kernel_rho(
    x1 = geno_preparator$Geno,
    x2 = geno_preparator$Geno,
    rho = kernel_rho
  )
  # Only in tunning the matrix has to be raised to rho
  if (raise_to_rho) {
    GenoKernel <- GenoKernel^rho
  }
  GenoLine <- Line %*% GenoKernel %*% t(Line)

  eta$Line <- list(x = GenoLine, model = predictors$line)

  Env <- model.matrix(~ 0 + Env, Pheno)
  Env <- Env %*% t(Env) / ncol(Env)

  if (!is.null(predictors$env)) {
    eta$Env <- list(x = Env, model = predictors$env)
  }

  if (!is.null(predictors$envxline)) {
    EnvGenoLine <- Env * GenoLine
    eta$EnvxLine <- list(x = EnvGenoLine, model = predictors$envxline)
  }

  return(eta)
}

radial_tuner_initialize <- function(...,
                                    model_iterations_number,
                                    burn_in,
                                    thinning,
                                    Pheno,
                                    y,
                                    geno_preparator,
                                    predictors) {
  super$initialize(
    ...,
    x = Pheno,
    y = y,
    training_function = NULL,
    predict_function = NULL
  )

  self$predictors <- predictors
  self$model_iterations_number <- model_iterations_number
  self$burn_in <- burn_in
  self$thinning <- thinning

  self$Pheno <- Pheno
  self$y <- y
  self$geno_preparator <- geno_preparator

  new_order <- order(self$Pheno$Env, self$Pheno$Line)
  self$Pheno <- self$Pheno[new_order, ]
  self$y <- get_records(self$y, new_order)

  unique_sorted_lines <- sort(unique(as.character(Pheno$Line)))
  self$geno_preparator$preprocess(unique_sorted_lines)
}

radial_eval_one_fold <- function(fold, combination) {
  ETA <- prepare_eta(
    self$Pheno,
    self$geno_preparator,
    combination$rho,
    self$predictors,
    raise_to_rho = TRUE
  )
  y_na <- self$y
  y_testing <- get_records(self$y, fold$testing)
  if (has_dims(y_na)) {
    y_na[fold$testing, ] <- NA
  } else {
    y_na[fold$testing] <- NA
  }

  model <- bayesian_model(
    x = ETA,
    y = y_na,
    iterations_number = self$model_iterations_number,
    burn_in = self$burn_in,
    thinning = self$thinning,
    verbose = FALSE
  )
  predictions <- predict(model, fold$testing)

  loss <- wrapper_loss(
    observed = y_testing,
    predictions = predictions,
    tuner = self
  )

  return(loss)
}

# This function access self parameters because it is not used in tuner
train_radial_bayes <- function(x, y, fit_params, predictors) {
  new_order <- order(self$Pheno$Env, self$Pheno$Line)
  self$Pheno <- self$Pheno[new_order, ]
  self$y <- get_records(self$y, new_order)

  # Fit params contains optimal rho
  ETA <- prepare_eta(
    Pheno = self$Pheno,
    geno_preparator = self$geno_preparator,
    rho = fit_params$rho,
    predictors = predictors,
    raise_to_rho = FALSE
  )
  y_na <- self$y
  if (!is_empty(self$testing_indices)) {
    if (self$is_multivariate) {
      y_na[self$testing_indices, ] <- NA
    } else {
      y_na[self$testing_indices] <- NA
    }
  }

  model <- bayesian_model(
    x = ETA,
    y = y_na,
    iterations_number = self$fit_params$iterations_number,
    burn_in = self$fit_params$burn_in,
    thinning = self$fit_params$thinning,
    verbose = FALSE
  )

  return(model)
}
