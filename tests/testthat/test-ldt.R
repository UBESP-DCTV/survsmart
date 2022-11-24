test_that("ldt works", {
  # setup
  db <- db_dtr2
  db_wrongcode <- db |>
    dplyr::mutate(
      X = .data[["X"]] - 1,
      Z = .data[["Z"]] - 1
    )

  # eval
  res <- ldt(db)

  # test
  expect_snapshot_value(res, style = "serialize")
  expect_error(ldt(db_wrongcode), "All elements must be > 0")
})

test_that("ldt throws correct errors", {
  # setup
  db <- db_dtr2

  # test
  expect_error(ldt(db, -1), "> 0")
  expect_error(ldt(db, "2"), "numeric")
  expect_error(ldt(db, NULL), "NULL")
})
