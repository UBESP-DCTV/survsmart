test_that("print works for ldt", {
  # setup
  res_ldt <- ldt(db_dtr2)

  # test
  expect_snapshot_output(print(res_ldt))
})
