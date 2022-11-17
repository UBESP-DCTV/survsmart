test_that("extractors works", {
  # setup
  res <- ldt(db_dtr2)

  # eval
  cen <- censor(res)
  rt <- risk_table(res)
  est <- estimate(res)

  # test
  expect_tibble(cen)
  expect_tibble(rt)
  expect_list(est)
})
