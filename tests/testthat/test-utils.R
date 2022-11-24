test_that("check_dtr_db works", {
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


test_that("which_first_min_dist_from works", {
  # setup
  t <- 4
  first <- 4:5
  odd <- 3:5
  even <- c(2.5, 3.5, 4.5, 5.5)
  l_asym <- c(1:3, 6:7)
  r_asym <- c(1:2, 5:7)
  l_asym_unord <- c(1:2, 6, 3, 7)
  r_asym_unord <- c(1, 5, 2, 6:7)


  # eval
  res <- list(
    "first" = first , "odd" = odd, "even" = even,
    "l_asym" = l_asym, "r_asym" = r_asym,
    "l_asym_unord" = l_asym_unord, "r_asym_unord" = r_asym_unord
  ) |>
    purrr::map(which_first_min_dist_from, t)

  # expectations
  expect_equal(res[["odd"]], 2)
  expect_equal(res[["even"]], 2)
  expect_equal(res[["l_asym"]], 3)
  expect_equal(res[["r_asym"]], 2) # res must be lte t
  expect_equal(res[["l_asym_unord"]], 4)
  expect_equal(res[["r_asym_unord"]], 3) # res must be lte t
})
