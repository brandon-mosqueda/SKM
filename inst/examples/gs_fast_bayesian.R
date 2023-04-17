data(Maize)

folds <- cv_kfold(nrow(Maize$Pheno), k = 5)

results <- gs_fast_bayesian(
  Maize$Pheno,
  Maize$Geno,

  traits = "Y",
  folds = folds,

  is_multitrait = FALSE,

  iterations_number = 10,
  burn_in = 5,
  thinning = 5,

  seed = NULL,
  verbose = TRUE
)

print(results)
#> $Predictions
#> # A tibble: 2,888 × 6
#>   Trait Fold  Line      Env   Observed Predicted
#>   <fct> <fct> <fct>     <fct>    <dbl>     <dbl>
#> 1 Y     1     L001_L003 1      -0.0274      7.11
#> 2 Y     1     L001_L012 1       0.574       3.89
#> 3 Y     1     L001_L023 1      -0.0636      3.95
#> 4 Y     1     L001_L033 1       0.215       2.05
#> 5 Y     1     L001_L034 1       0.299       2.70
#> # … with 2,883 more rows
#> # ℹ Use `print(n = ...)` to see more rows
#>
#> $traits: Y
#>
#> $is_multitrait: FALSE
#>
#> $folds: 5
#>
#> $execution_time: 0.841673 mins
#>
#> $Pheno
#>         envs_num: 4
#>         lines_num: 722
#>         rows_num: 2888
#>
#> $Geno
#>         rows_num: 722
#>         cols_num: 722
#>
#> $model_name: BGBLUP
#>
#> $iterations_number: 10
#>
#> $burn_in: 5
#>
#> $thinning: 5
