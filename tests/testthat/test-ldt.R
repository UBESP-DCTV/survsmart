test_that("ldt works", {
  # setup
  db <- db_dtr2

  # eval
  res <- ldt(db)

  # test
  expect_snapshot_value(res, style = "serialize")
})
