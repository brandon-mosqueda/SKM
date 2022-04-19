#' @details
#' In univariate models with a numeric response variable, Mean
#' Squared Error ([mse()]) is used by default as loss function. In univariate
#' models with a categorical response variable, either binary or with more than
#' two categories, accuracy ([accuracy()]) is used by default. You can change
#' the default loss function used in tuning with the `tune_loss_function`
#' parameter.
