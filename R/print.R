#' @export
print.ldt <- function(x, ...) {
  c <- get_call(x)
  if (!is.null(c)) {
    cat("Call: ")
    dput(c)
    cat("\n")
  }

  t <- risk_table(x)[["time"]]
  surv <- estimate(x)

  x |>
    dplyr::mutate(
      median = time_at(t, surv, "median"),
      LCL95 = time_at(t, surv, "lcl"),
      UCL95 = time_at(t, surv, "ucl")
    ) |>
    tibble::as_tibble()
}


time_at <- function(t, x, what = c("median", "lcl", "ucl")) {
  what <- match.arg(what)
  traslation <- switch(
    what,
    median = 0,
    lcl = -1,
    ucl = 1
  )

  purrr::map(x, ~ {
    c(
      t[which_first_min_dist_from(.x[["SURV1"]] + traslation * 1.96 * .x[["SE1"]] , 0.5)],
      t[which_first_min_dist_from(.x[["SURV2"]] + traslation * 1.96 * .x[["SE2"]], 0.5)]
    )
  }) |> unlist()
}
