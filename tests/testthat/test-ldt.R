test_that("ldt works", {
  # setup
  db <- db_dtr2

  # eval
  res <- ldt(db)

  # test
  expect_snapshot_value(res, style = "serialize")
})

test_that("ldt throws correct errors", {
  # setup
  db <- db_dtr2
  missing_var_db <- data.frame(
    X = rep(NA, 4),
    R = 1:4,
    Z = rep(1, 4),
    U = rep(2, 4),
    delta = rep(1, 4)
  )

  # eval


  # test
  expect_error(ldt(db, -1), "> 0")
  expect_error(ldt(db, "2"), "numeric")
  expect_error(ldt(db, NULL), "NULL")
  expect_error(ldt(missing_var_db), "only missing")
})
