test_that("multiplication works", {
  # setup
  missing_var_db <- data.frame(
    X = 1:4,
    R = 1:4,
    Z = rep(1, 4),
    U = rep(2, 4),
    delta = rep(1, 4)
  )

  # test
  ldt(dplyr::mutate(missing_var_db, X = NA)) |>
    expect_error("only missing")

  ldt(dplyr::mutate(missing_var_db, R = NA)) |>
    expect_error("only missing")

  ldt(dplyr::mutate(missing_var_db, Z = NA)) |>
    expect_error("only missing")

  ldt(dplyr::mutate(missing_var_db, U = NA)) |>
    expect_error("only missing")

  ldt(dplyr::mutate(missing_var_db, delta = NA)) |>
    expect_error("only missing")
})
