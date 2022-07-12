# Sparse Kernel Methods (SKM)

SKM is an R package for machine learning that includes functions for model training, tuning, prediction, metrics evaluation and sparse kernels computation. The main goal of this package is not to provide a full toolkit for data analysis but focus specifically in 7 supervised type of models: 1) generalized boosted machines, which internally uses [gbm](https://cran.r-project.org/web/packages/gbm/index.html) package, 2) generalized linear models of [glmnet](https://cran.r-project.org/web/packages/glmnet/) package, 3) support vector machines of [e1071](https://cran.r-project.org/web/packages/e1071/) package, 4) random forest of [randomForestSRC](https://cran.r-project.org/web/packages/randomForestSRC/) package, 5) bayesian regression models of [BGLR](https://cran.r-project.org/web/packages/BGLR/) package, 6) deep neural networks of [keras](https://cran.r-project.org/web/packages/keras/) package and 7) partial least squares, which uses [pls](https://cran.r-project.org/web/packages/pls/) package.

The model functions in SKM were designed keeping in mind simplicity, so the parameters, hyperparameters and tuning specifications are defined directly when calling the function and the user only have to see one example to understand how the package works. The most important hyperparameters of each model can be tuned with two different methods, grid search and Bayesian optimization.

The SKM paper can be consulted in the following link: [https://doi.org/10.3389/fgene.2022.887643](https://doi.org/10.3389/fgene.2022.887643).

## Installation

Currently this package is only available in Github and can be installed with the following commands:

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("brandon-mosqueda/SKM")
```

## Authors

* Osval Antonio Montesinos López (author)
* Brandon Alejandro Mosqueda González (author, maintainer)
* Abel Palafox González (author)
* Abelardo Montesinos López (author)
* José Crossa (author)

## Citation

First option, by paper:

    @article{10.3389/fgene.2022.887643,
      author={Montesinos López, Osval Antonio and
        Mosqueda González, Brandon Alejandro and
        Palafox González, Abel and
        Montesinos López, Abelardo and
        Crossa, José},
      title={A General-Purpose Machine Learning R Library for
        Sparse Kernels Methods With an Application for
        Genome-Based Prediction},
      journal={Frontiers in Genetics},
      volume={13},
      year={2022},
      url={https://www.frontiersin.org/articles/10.3389/fgene.2022.887643},
      doi={10.3389/fgene.2022.887643},
      issn={1664-8021}
    }

Second option, by package:

```r
citation("SKM")

## To cite package 'SKM' in publications use:
##
##  Montesinos López O, Mosqueda González B, Palafox González A,
##  Montesinos López A, Crossa J (2022). _SKM: Sparse Kernels Methods_.
##  R package version 1.0.
##
## A BibTeX entry for LaTeX users is
##
##  @Manual{,
##    title = {SKM: Sparse Kernels Methods},
##    author = {Osval Antonio {Montesinos López} and
##    Brandon Alejandro {Mosqueda González} and
##    Abel {Palafox González} and
##    Abelardo {Montesinos López} and
##    José Crossa},
##    year = {2022},
##    note = {R package version 1.0},
##  }
```
