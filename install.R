# Script to install all the required packages for SKM library

is_installed <- function(package) {
  return(all(package %in% rownames(installed.packages())))
}

is_windows_os <- function() {
  return(.Platform$OS.type == "windows")
}

ok <- TRUE

if (is_windows_os()) {
  if (!is_installed("installr")) {
    install.packages("installr")
  }

  ok <- install.Rtools(check = TRUE, check_r_update = FALSE)
}

if (ok) {
  if (!is_installed("devtools")) {
    install.packages("devtools")
  }

  devtools::install_github("gdlc/BGLR-R", upgrade = "default")
  devtools::install_github("rstudio/tensorflow", upgrade = "default")
  devtools::install_github("rstudio/keras", upgrade = "default")
  devtools::install_github("brandon-mosqueda/SKM", upgrade = "default")
}
