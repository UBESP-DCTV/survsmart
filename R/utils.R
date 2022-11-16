check_dtr_db <- function(db) {
  checkmate::assert_data_frame(
    db,
    types = c(rep("integerish", 3), "numeric", " integerish"),
    all.missing = FALSE,
    min.rows = 4L,
    ncols = 5L
  )
  checkmate::assert_names(
    names(db),
    permutation.of = c("X", "R", "Z", "U", "delta")
  )
}

which_first_min_dist_from <- function(vec, ref) {
  checkmate::assertNumeric(vec)
  checkmate::assert_number(ref)

  vec[vec > ref] <- NA
  order(abs(vec - ref))[[1]]
}
