# Sparse Kernel Methods (SKM)

SKM is an R package for machine learning that includes functions for model training, tuning, prediction, metrics evaluation and sparse kernels computation. The main goal of this package is not to provide a full toolkit for data analysis but focus specifically in 7 supervised type of models: 1) generalized boosted machines, which internally uses [gbm](https://cran.r-project.org/web/packages/gbm/index.html) package, 2) generalized linear models of [glmnet](https://cran.r-project.org/web/packages/glmnet/) package, 3) support vector machines of [e1071](https://cran.r-project.org/web/packages/e1071/) package, 4) random forest of [randomForestSRC](https://cran.r-project.org/web/packages/randomForestSRC/) package, 5) bayesian regression models of [BGLR](https://cran.r-project.org/web/packages/BGLR/) package, 6) deep neural networks of [keras](https://cran.r-project.org/web/packages/keras/) package and 7) partial least squares, which uses [pls](https://cran.r-project.org/web/packages/pls/) package.

The model functions in SKM were designed keeping in mind simplicity, so the parameters, hyperparameters and tuning specifications are defined directly when calling the function and the user only have to see one example to understand how the package works. The most important hyperparameters of each model can be tuned with two different methods, grid search and Bayesian optimization.

## Installation

Currently this package is only available in Github.

```r
devtools::install_github("brandon-mosqueda/SKM")
```

## Authors

* Osval Antonio Montesinos López (author)
* Brandon Alejandro Mosqueda González (author, maintainer)
* Abelardo Montesinos López (author)
* José Crossa (author)
