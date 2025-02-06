#' @import dplyr

#' @include utils.R
#' @include metrics.R
#' @include validator.R

lm_intercept <- function(x, y) {
  if (all(is.na(x)) || all(is.na(y))) {
    return(NaN)
  }
  Data <- data.frame(x = x, y = y)

  return(lm(y ~ x, data = Data)$coefficients[1])
}

lm_slope <- function(x, y) {
  if (all(is.na(x)) || all(is.na(y))) {
    return(NaN)
  }
  data <- data.frame(x = x, y = y)

  return(lm(y ~ x, data = data)$coefficients[2])
}

compute_standard_errors <- function(summary, digits) {
  summary <- summary %>%
    # Metrics mean and standard errors
    summarise_if(
      is.numeric,
      list(
        MEAN = ~ mean(., na.rm = TRUE),
        SE = ~ sd(., na.rm = TRUE) / sqrt(n())
      ),
      .groups = "keep"
    ) %>%
    mutate_if(is.numeric, list(~ round(., digits))) %>%
    as_tibble()

  colnames(summary) <- gsub("_MEAN", "", colnames(summary))

  return(summary)
}

numeric_metrics <- function(predictions) {
  summary <- summarise(
    predictions,

    MSE = mse(Observed, Predicted),
    RMSE = rmse(Observed, Predicted),
    NRMSE = nrmse(Observed, Predicted),
    MAE = mae(Observed, Predicted),
    APC = pearson(Predicted, Observed),
    Intercept = lm_intercept(Predicted, Observed),
    Slope = lm_slope(Predicted, Observed),
    R2 = pearson(Predicted, Observed)^2,
    MAAPE = maape(Observed, Predicted),
    Best10 = best_lines_match(
      tibble(Line, Observed, Predicted),
      percentage = 10
    ),
    Best20 = best_lines_match(
      tibble(Line, Observed, Predicted),
      percentage = 20
    ),

    .groups = "drop"
  )

  return(summary)
}

categorical_metrics <- function(predictions) {
  classes <- levels(predictions$Predicted)

  summary <- summarise(
    predictions,

    Accuracy = accuracy(Observed, Predicted),
    Kappa = kappa_coeff(Observed, Predicted),
    BrierScore = brier_score(
      Observed,
      select_at(across(everything()), classes)
    ),

    .groups = "drop"
  )

  return(summary)
}

numeric_summarise_line <- function(predictions, digits, group_cols = "Line") {
  Line <- predictions %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      Observed = mean(Observed, na.rm = TRUE),
      Predicted = mean(Predicted, na.rm = TRUE),
      Difference = abs(Observed - Predicted),
      .groups = "drop"
    ) %>%
    arrange(Difference) %>%
    mutate_if(is.numeric, list(~round(., digits))) %>%
    as_tibble()

  return(Line)
}

categorical_summarise_line <- function(predictions,
                                       digits,
                                       group_cols = "Line") {
  rmean <- function(x) round(mean(x), digits)

  classes <- levels(predictions$Predicted)

  Line <- predictions %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      Observed = math_mode(Observed, allow_multimodal = FALSE) %>%
        factor(classes),
      Predicted = math_mode(Predicted, allow_multimodal = FALSE) %>%
        factor(classes),
      across(all_of(classes), rmean),
      .groups = "drop"
    ) %>%
    as_tibble()

  return(Line)
}

reorder_se_cols <- function(Data, first_cols) {
  se_cols <- select(Data, contains("_SE")) %>% colnames()

  reordered_cols <- sort(c(
    gsub("_SE", "", se_cols),
    se_cols
  ))

  return(select(Data, all_of(c(first_cols, reordered_cols))))
}

#' @title Summaries for Genomic Selection
#'
#' @description
#' Generates summaries of predictions from different folds for Genomic Selection
#' analysis.
#'
#' @param predictions (`data.frame`) The genomic, observed and predicted data.
#'   The `data.frame` must contain the columns `Fold`, `Line`, `Env`, `Observed`
#'   and `Predicted`. If the response variable is categorical make sure to set
#'   Observed and Predicted as factor. For categorical response variables it is
#'   also necessary to include a column for each class in the response variable
#'   with the predicted probabilities, the column must have the class as name.
#'   If `predictions` contains a column nameed `Trait`, the summaries will be
#'   computed and saved per trait.
#' @param save_at (`character(1)`) The directory's name where the summaries and
#'   predictions are going to be saved as CSV. If `NULL` is provided, the
#'   results are only returned but not saved. `NULL` by default.
#' @param digits (`numeric(1)`) Digits of precision to use in the summaries. 4
#'   by default.
#'
#' @return
#' A `list` with 3 summaries: `"line"`, `"env"` and `"fold"`.
#'
#' @examples
#' \dontrun{
#' # For a continuous response ------------------------------------------------
#' set.seed(1)
#'
#' # Simulated data
#' predictions <- data.frame(
#'   Fold = rep(c("F1", "F2"), each = 24),
#'   Env = rep(c("E1", "E2"), each = 12),
#'   Line = rep(c("L1", "L2", "L3"), 16),
#'   Observed = rnorm(48, 10, 1),
#'   Predicted = rnorm(48, 10, 1)
#' )
#'
#' summaries <- gs_summaries(predictions, save_at = "numeric_summaries")
#' summaries$line
#' summaries$env
#' summaries$fold
#'
#' # For a categorical response ------------------------------------------------
#' set.seed(2)
#'
#' # Simulated data
#' predictions <- data.frame(
#'     Fold = rep(c("F1", "F2"), each = 24),
#'     Env = rep(c("E1", "E2"), each = 12),
#'     Line = rep(c("L1", "L2", "L3"), 16),
#'     Observed = sample(c("A", "B"), 24, TRUE),
#'     A = runif(48, 0, 1)
#'   ) %>%
#'   dplyr::mutate(
#'     B = 1 - A,
#'     Predicted = factor(ifelse(A > 0.5, "A", "B")),
#'     Observed = factor(Observed)
#'   )

#' summaries <- gs_summaries(predictions, save_at = "categorical_summaries")
#' summaries$line
#' summaries$env
#' summaries$fold
#' }
#'
#' @export
gs_summaries <- function(predictions, save_at = NULL, digits = 4) {
  assert_gs_summary(predictions, save_at, digits)

  if ("Trait" %in% colnames(predictions)) {
    traits <- unique(predictions$Trait)

    summaries <- lapply(traits, function(trait) {
      trait_predictions <- predictions %>%
        filter(Trait == trait)

      gs_summaries_single(
        predictions = trait_predictions,
        save_at = if(is.null(save_at)) NULL else file.path(save_at, trait),
        digits = digits
      )
    })
    names(summaries) <- traits
  } else {
    summaries <- gs_summaries_single(
      predictions = predictions,
      save_at = save_at,
      digits = digits
    )
  }

  class(summaries) <- "GSSummaries"

  return(summaries)
}

gs_summaries_single <- function(predictions, save_at = NULL, digits = 4) {
  is_categorical <- is.factor(predictions$Observed)

  if (is_categorical) {
    classes <- get_levels(predictions$Observed, predictions$Predicted)
    predictions <- predictions %>%
      mutate(
        Observed = factor(Observed, levels = classes),
        Predicted = factor(Predicted, levels = classes)
      )
  }

  summary_function <- ifelse(
    is_categorical,
    categorical_metrics,
    numeric_metrics
  )
  line_summary_function <- ifelse(
    is_categorical,
    categorical_summarise_line,
    numeric_summarise_line
  )

  LineSummary <- line_summary_function(predictions, digits)

  AcrossNewSummary <- predictions %>%
    line_summary_function(digits = digits, group_cols = c("Fold", "Line")) %>%
    group_by(Fold) %>%
    summary_function() %>%
    select(-Fold) %>%
    compute_standard_errors(digits)

  EnvSummary <- predictions %>%
    group_by(Env, Fold) %>%
    summary_function() %>%
    group_by(Env) %>%
    compute_standard_errors(digits) %>%
    ungroup()

  EnvSummary <- EnvSummary %>%
    bind_rows(
      summarise_if(EnvSummary, is.numeric, ~ mean(., na.rm = TRUE)) %>%
        mutate(Env = "Across-env"),
      AcrossNewSummary %>%
        mutate(Env = "Across-env-new"),
    ) %>%
    reorder_se_cols(first_cols = c("Env"))


  FoldSummary <- predictions %>%
    group_by(Fold) %>%
    summary_function() %>%
    ungroup()

  FoldSummary <- FoldSummary %>%
    bind_rows(
      compute_standard_errors(FoldSummary, digits) %>%
        mutate(Fold = "Across-fold"),
      AcrossNewSummary %>%
        mutate(Fold = "Across-fold-new"),
    ) %>%
    reorder_se_cols(first_cols = c("Fold"))

  if (!is.null(save_at)) {
    mkdir(save_at)

    write_csv(predictions, file.path(save_at, "predictions.csv"))
    write_csv(LineSummary, file.path(save_at, "line_summary.csv"))
    write_csv(EnvSummary, file.path(save_at, "env_summary.csv"))
    write_csv(FoldSummary, file.path(save_at, "fold_summary.csv"))
  }

  return(list(
    line = LineSummary,
    env = EnvSummary,
    fold = FoldSummary
  ))
}

#' @export
print.GSSummaries <- function(summaries) {
  invisible(cat(str(summaries, 2)))
}
