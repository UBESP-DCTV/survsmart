if (interactive()) {
  suppressPackageStartupMessages(suppressWarnings({
    library(devtools)
    library(testthat)
    library(checkmate)
    library(usethis)
  }))

  .add_function <- function(fct_name) {
    fct_name |>
      usethis::use_test() |>
      basename() |>
      usethis::use_r()
  }
}
