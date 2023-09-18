#' @importFrom tensorflow set_random_seed

#' @include utils.R
#' @include validator.R
#' @include deep_learning_model.R

#' @title Fit a Deep Learning Model
#'
#' @templateVar ClassName DeepLearningModel
#' @templateVar XType `matrix`
#' @templateVar YType `vector` or `matrix`
#' @templateVar refFunction keras::keras_model_sequential()
#'
#' @description
#' `deep_learning()` is a wrapper of the [keras::keras_model_sequential()]
#' function to fit a deep learning model with the ability to tune the
#' hyperparameters with grid search or bayesian optimization in a simple way.
#' You can fit univariate and multivariate models for numeric and/or
#' categorical response variables.
#' @template tunable-description
#'
#' @template x-matrix-param
#' @param y (`data.frame` | `vector` | `matrix`) The response (dependent)
#'   variable(s). If it is a `data.frame` or a `matrix` with 2 or more columns,
#'   a multivariate model is assumed, a univariate model otherwise. In
#'   univariate models if `y` is `character`, `logical` or `factor` a
#'   categorical response is assumed. When the response is categorical with only
#'   two classes a binary response is assumed, with more than two classes a
#'   categorical response. When the response variable is numeric with only
#'   integers values greater or equals than zero a count response is
#'   assumed, a continuous response otherwise. In multivariate models response
#'   variables can be of different types for mixed models.
#' @param learning_rate (`numeric`) (__tunable__) This hyperparameter controls
#'   how much to change the model in response to the estimated error each time
#'   the model weights are updated. 0.001 by default.
#' @param epochs_number (`numeric`) (__tunable__) An arbitrary cutoff, generally
#'   defined as "one pass over the entire dataset", used to separate training
#'   into distinct phases, which is useful for logging and periodic evaluation.
#'   500 by default.
#' @param batch_size (`numeric`) (__tunable__) A hyperparameter of gradient
#'   descent that controls the number of training samples to work through before
#'   the model's internal parameters are updated. 32 by default.
#' @param layers (`list`) The hidden layers. It must be a `list` of `list`'s
#' where each entry will represent a hidden layer in the neural network. Each
#' inner `list` can contain the following fields with a vector of values:
#'
#' * `"neurons_number"`: (`numeric`) (__tunable__) The number of neurons in that
#'   layer. 50 by default.
#' * `"neurons_proportion"`: (`numeric`) (__tunable__) Similar to
#'   `"neurons_number"` but the provided values will be the proportion specified
#'   times the number of columns in `x`, so a value of 1 means "use as many
#'   neurons as columns in `x`", 0.5 means use as neurons number the half of
#'   number of columns in `x`. This is combined with the values of
#'   `"neurons_number"` for tuning. `NULL` by default.
#' * `"activation"`: (`character`) (__tunable__) (case not sensitive) The name
#'   of the activation function to apply in this layer. The available activation
#'   functions are `"linear"`, `"relu"`, `"elu"`, `"selu"`, `"hard_sigmoid"`,
#'   `"sigmoid"`, `"softmax"`, `"softplus"`, `"softsign"`, `"tanh"`,
#'   `"exponential"`. This hyperparameter can only be tuned with grid search
#'   tuning, with bayesian optimization a fixed value have to be provided.
#'   `"relu"` by default.
#' * `"dropout"`: (`numeric`) (__tunable__) The proportion of neurons randomly
#'   selected and set to 0 at each step during training process, which helps
#'   prevent overfitting. 0 by default.
#' * `"lasso_penalty"`: (`numeric`) (__tunable__) The regularization value
#'   between \[0, 1\] for penalizing that layer with Lasso (a.k.a L1) penalty. 0
#'   by default (no penalty).
#' * `"ridge_penalty"`: (`numeric`) (__tunable__) The regularization value
#'   between \[0, 1\] for penalizing that layer with Ridge (a.k.a L2) penalty.
#'   Note that if both penalization params (Ridge and Lasso) are sent, the
#'   ElasticNet penalization is implemented, that is a combination of both of
#'   them. 0 by default (no penalty).
#'
#' You can provide as many `list`'s as you want, each of them representing a
#' hidden layer and you do not need to provide all the parameters. If one
#' parameter is not provided, the default value described above is used. By
#' default the next `list` is used:
#'
#' `list(
#'   list(
#'     neurons_number = 50,
#'     neurons_proportion = NULL,
#'     activation = "relu",
#'     dropout = 0,
#'     ridge_penalty = 0,
#'     lasso_penalty = 0
#'   )
#' )`
#' @param output_penalties (`list`) The penalty values for the output layer. The
#' list can contain the following two fields:
#'
#' * `"lasso_penalty"`: (`numeric`) (__tunable__) The regularization value
#'   between \[0, 1\] for penalizing that layer with Lasso (a.k.a L1) penalty. 0
#'   by default (no penalty).
#' * `"ridge_penalty"`: (`numeric`) (__tunable__) The regularization value
#'   between \[0, 1\] for penalizing that layer with Ridge (a.k.a L2) penalty.
#'   Note that if both penalization params (Ridge and Lasso) are sent, the
#'   ElasticNet penalization, is implemented that is a combination of both of
#'   them. 0 by default (no penalty).
#'
#' You do not have to provide the two values, if one of them is not provided the
#' default value is used. By default the next `list` is used:
#'
#' `list(
#'   ridge_penalty = 0,
#'   lasso_penalty = 0
#' )`
#' @template cv-tune-params
#' @param optimizer (`character(1)`) (case not sensitive) Algorithm used to
#'   reduce the loss function and update the weights in backpropagation. The
#'   available options are `"adadelta"`, `"adagrad"`, `"adamax"`, `"adam"`,
#'   `"nadam"`, `"rmsprop"` and `"sgd"`. `"adam"` by default.
#' @param loss_function (`character(1)`) (case not sensitive) The name of the
#'   loss function the model will seek to minimize during training and tuning.
#'   You can find the complete list of available loss functions in the Details
#'   section below. This parameter can be used only in univariate analysis.
#'   `NULL` by default which selects one automatically based on the type of the
#'   response variable `y`: `"mean_squared_error"` for continuous, `"poisson"`
#'   for counts, `"binary_crossentropy"` for binary and
#'   `"categorical_crossentropy"` for categorical.
#' @param with_platt_scaling (`logical(1)`) Should Platt scaling be used to fit
#'   the model and adjust the predictions? Only available for univariate models
#'   with a numeric or binary response variable. For more information, see
#'   Details section below. `FALSE` by default.
#' @param platt_proportion (`numeric(1)`) The proportion of individuals used to
#'   fit the linear model required for Platt scaling. Note that this parameter
#'   is used only when `with_platt_scaling` is `TRUE`. 0.3 by default.
#' @param shuffle (`logical(1)`) Should the training data be shuffled before
#'   each epoch? `TRUE` by default.
#' @param early_stop (`logical(1)`) Should the model stop training when the loss
#'   function has stopped improving? `FALSE` by default.
#' @param early_stop_patience (`numeric(1)`) The number of epochs with no
#'   improvement after which training will be stopped. Note that this parameter
#'   is used only when `early_stop` is `TRUE`. 50 by default.
#' @template other-base-params
#'
#' @template details-no-variance
#' @template details-remove-nas
#' @template details-tuning
#' @details
#' __Important:__ Unlike the other models, when tuning deep learning models
#' steps 6 and 7 are omited in the algorithm, instead `train` and `test`
#' datasets are sent to `keras`, the first one to fit the model and the second
#' one to compute the loss function at the end of each epoch, so at the end, the
#' saved value in step 8 is the validation loss value returned by `keras` in the
#' last epoch. `tune_loss_function` parameter cannot be used in `deep_learning`
#' function since the same loss function evaluated at each epoch and specified
#' in `loss_function` parameter is used for tuning too.
#'
#' ## Last (output) layer
#'
#' By default this function selects the activation function and the number of
#' neurons for the last layer of the model based on the response variable(s)
#' type(s). For continuous responses the `"linear"` (identity) activation
#' function is used with one neuron, for count responses the `"exponential"`
#' with one neuron, for binary responses the `"sigmoid"` with one neuron and for
#' categorical responses `"softmax"` with as many neurons as number of
#' categories.
#'
#' ## Loss functions
#'
#' The available options of the `loss_function` parameter are:
#'
#' _Probabilistic losses_
#'
#' * `"binary_crossentropy"`
#' * `"categorical_crossentropy"`
#' * `"sparse_categorical_crossentropy"`
#' * `"poisson"`
#' * `"kl_divergence"`
#'
#' _Regression losses_
#'
#' * `"mean_squared_error"`
#' * `"mean_absolute_error"`
#' * `"mean_absolute_percentage_error"`
#' * `"mean_squared_logarithmic_error"`
#' * `"cosine_similarity"`
#' * `"huber"`
#' * `"log_cosh"`
#'
#' _Hinge losses for "maximum-margin" classification_
#'
#' * `"hinge"`
#' * `"squared_hinge"`
#' * `"categorical_hinge"`
#'
#' ## Platt scaling
#'
#' It is a way of improving the training process of deep learning models that
#' uses a calibration based on a model that is already trained and applied via
#' a post-processing operation.
#'
#' After tuninig, Platt scaling calibration divides the dataset into `Training`
#' and `Calibration` datasets, then it uses `Training` to fit the deep learning
#' model with the best hyperparameters combination and with this model computes
#' the predictions of the `Calibration` dataset. Finally with the predicted and
#' true values, a linear model is fitted (observed in function of predicted),
#' this linear model corresponds to the calibration and when a new prediction is
#' going to be made, first the deep learning model is used and the resulting
#' predicted value is calibrated with the linear model.
#'
#' Note that Platt scaling calibration only works for numeric and binary
#' response variables of univariate models.
#'
#' @template return-model
#'
#' @seealso [predict.Model()]
#' @family models
#'
#' @example inst/examples/deep_learning.R
#'
#' @export
deep_learning <- function(x, y,

                          learning_rate = 0.001,
                          epochs_number = 500,
                          batch_size = 32,
                          layers = list(
                            list(
                              neurons_number = 50,
                              neurons_proportion = NULL,
                              activation = "relu",
                              dropout = 0,
                              ridge_penalty = 0,
                              lasso_penalty = 0
                            )
                          ),
                          output_penalties = list(
                            ridge_penalty = 0,
                            lasso_penalty = 0
                          ),

                          tune_type = "Grid_search",
                          tune_cv_type = "K_fold",
                          tune_folds_number = 5,
                          tune_testing_proportion = 0.2,
                          tune_folds = NULL,
                          tune_grid_proportion = 1,
                          tune_bayes_samples_number = 10,
                          tune_bayes_iterations_number = 10,

                          optimizer = "adam",
                          loss_function = NULL,
                          with_platt_scaling = FALSE,
                          platt_proportion = 0.3,
                          shuffle = TRUE,
                          early_stop = FALSE,
                          early_stop_patience = 50,

                          validate_params = TRUE,
                          seed = NULL,
                          verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  is_multivariate <- NCOL(y) > 1

  if (validate_params) {
    validate_deep_learning(
      x = x,
      y = y,
      is_multivariate = is_multivariate,

      learning_rate = learning_rate,
      epochs_number = epochs_number,
      batch_size = batch_size,
      layers = layers,
      output_penalties = output_penalties,

      tune_type = tune_type,
      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,
      tune_folds = tune_folds,
      tune_grid_proportion = tune_grid_proportion,
      tune_bayes_samples_number = tune_bayes_samples_number,
      tune_bayes_iterations_number = tune_bayes_iterations_number,

      optimizer = optimizer,
      loss_function = loss_function,
      with_platt_scaling = with_platt_scaling,
      platt_proportion = platt_proportion,
      shuffle = shuffle,
      early_stop = early_stop,
      early_stop_patience = early_stop_patience,

      seed = seed,
      verbose = verbose
    )
  }

  old_random_state <- NULL
  if (!is.null(seed)) {
    old_random_state <- get_rand_state()

    # set_random_seed sets R seed too
    py_hush(set_random_seed(seed))
    warning(
      "When you use a seed GPU parallelism are disabled since it can result ",
      "in non-deterministic execution patterns, so if you have a GPU in your ",
      "computer and you want to use it for parallelism, do not use a seed."
    )
  }
  on.exit(set_rand_state(old_random_state))

  start_time <- Sys.time()

  model <- DeepLearningModel$new(
    x = x,
    y = y,
    is_multivariate = is_multivariate,

    learning_rate = learning_rate,
    epochs_number = epochs_number,
    batch_size = batch_size,
    layers = layers,
    output_penalties = output_penalties,

    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number,

    optimizer = optimizer,
    loss_function = loss_function,
    with_platt_scaling = with_platt_scaling,
    platt_proportion = platt_proportion,
    shuffle = shuffle,
    early_stop = early_stop,
    early_stop_patience = early_stop_patience
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(py_capture_output(py_suppress_warnings(model$fit())))

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
