data(Maize)

folds <- cv_kfold(nrow(Maize$Pheno), k = 5)

results <- gs_bayesian(
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
