test_that("wrse works", {
  # setup
  db <- db_dtr2
  db_wrongcode <- db |>
    dplyr::mutate(
      X = .data[["X"]] - 1,
      Z = .data[["Z"]] - 1
    )

  # eval
  res <- wrse(db)

  # test
  expect_snapshot_value(res, style = "serialize")
  expect_error(wrse(db_wrongcode), "All elements must be > 0")
})
