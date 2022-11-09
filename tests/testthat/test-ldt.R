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

  # test
  expect_error(ldt(db, -1), "> 0")
  expect_error(ldt(db, "2"), "numeric")
  expect_error(ldt(db, NULL), "NULL")
})
