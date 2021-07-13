#' @importFrom R6 R6Class
#' @importFrom e1071 svm

#' @include model.R

SVMModel <- R6Class(
  classname = "SVMModel",
  inherit = Model,
  public = list(
    # Properties --------------------------------------------------

    scale = NULL,
    svm_kernel = NULL,
    svm_degree = NULL,
    svm_gamma = NULL,
    svm_coef0 = NULL,
    cost = NULL,
    class_weights = NULL,
    cache_size = NULL,
    tolerance = NULL,
    epsilon = NULL,
    shrinking = NULL,
    fitted = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...,
                          scale,
                          svm_kernel,
                          svm_degree,
                          svm_gamma,
                          svm_coef0,
                          cost,
                          class_weights,
                          cache_size,
                          tolerance,
                          epsilon,
                          shrinking,
                          fitted,
                          na_action) {
      tunable_hyperparams <- c("svm_degree", "svm_gamma", "svm_coef0", "cost")

      super$initialize(
        ...,
        name = "SVM",
        is_multivariate = FALSE,
        tunable_hyperparams = tunable_hyperparams
      )

      self$scale <- scale
      self$svm_kernel <- svm_kernel
      self$svm_degree <- svm_degree
      self$svm_gamma <- svm_gamma
      self$svm_coef0 <- svm_coef0
      self$cost <- cost
      self$class_weights <- class_weights
      self$cache_size <- cache_size
      self$tolerance <- tolerance
      self$epsilon <- epsilon
      self$shrinking <- shrinking
      self$fitted <- fitted
    },
    predict = function(x) {
      x <- prepare_x(
        x = x,
        kernel = self$kernel,
        rows_proportion = self$rows_proportion,
        arc_cosine_deep = self$arc_cosine_deep,
        degree = self$degree,
        gamma = self$gamma,
        coef0 = self$coef0
      )

      predicted <- predict(self$fitted_model, x, probability = TRUE)

      if (is_class_response(self$responses[["y"]]$type)) {
        probabilities <- attr(predicted, "probabilities")
        attr(predicted, "probabilities") <- NULL
        names(predicted) <- NULL

        predicted <- list(
          predicted = predicted,
          probabilities = probabilities
        )
      } else {
        predicted <- list(predicted = predicted)
      }

      return(predicted)
    }
  ),
  private = list(
    # Methods --------------------------------------------------

    train = function(params, hyperparams) {
      model <- svm(
        x = params$x,
        y = params$y,
        scale = params$scale,
        kernel = params$svm_kernel,
        degree = params$svm_degree,
        gamma = params$svm_gamma,
        coef0 = params$svm_coef0,
        cost = params$cost,
        class_weights = params$class_weights,
        cache_size = params$cache_size,
        tolerance = params$tolerance,
        epsilon = params$epsilon,
        shrinking = params$shrinking,
        probability = TRUE,
        fitted = params$fitted
      )

      return(model)
    }
  )
)
