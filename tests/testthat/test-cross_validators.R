records_number <- nrow(iris)

basic_validation <- function(cross_validator,
                             folds,
                             records_number,
                             expected_folds_number,
                             expect_all_records = TRUE) {
  expect_class(cross_validator, "CrossValidator")
  expect_list(folds)

  expect_length(folds, expected_folds_number)
  expect_equal(cross_validator$folds_number, expected_folds_number)
  expect_equal(cross_validator$records_number, records_number)

  for (fold_num in 1:expected_folds_number) {
    current_fold <- folds[[fold_num]]
    expect_list(current_fold)
    expect_names(names(current_fold), permutation.of = c("training", "testing"))

    if (expect_all_records) {
      all_indices <- unlist(current_fold)
      expect_set_equal(all_indices, seq(records_number))
    }
  }
}

are_mutually_exclusive_folds <- function(folds) {
  all_testing_indices <- c()
  for (fold in folds) {
    all_testing_indices <- c(all_testing_indices, fold$testing)
  }

  return(!any(duplicated(all_testing_indices)))
}

has_similar_length <- function(folds) {
  testing_lengths <- c()
  for (fold in folds) {
    testing_lengths <- c(testing_lengths, fold$testing)
  }

  differences <- testing_lengths - max(testing_lengths)

  return(!any(differences > 1))
}

test_that("KFoldCV", {
  folds_number <- 5

  cross_validator <- KFoldCV$new(
    folds_number = folds_number,
    records_number = records_number
  )
  folds <- cross_validator$get_folds()

  basic_validation(
    cross_validator,
    folds,
    records_number = records_number,
    expected_folds_number = folds_number
  )

  expect_true(are_mutually_exclusive_folds(folds))
  expect_true(has_similar_length(folds))
})

test_that("RandomCV", {
  folds_number <- 5
  testing_proportion <- 0.35

  cross_validator <- RandomCV$new(
    folds_number = folds_number,
    records_number = records_number,
    testing_proportion = testing_proportion
  )
  folds <- cross_validator$get_folds()

  basic_validation(
    cross_validator,
    folds,
    records_number = records_number,
    expected_folds_number = folds_number
  )
})
