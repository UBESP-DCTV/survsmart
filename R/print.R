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
      median = purrr::map(surv, ~ {
        c(
          t[which_first_min_dist_from(.x[["SURV1"]], 0.5)],
          t[which_first_min_dist_from(.x[["SURV2"]], 0.5)]
        )
      }) |> unlist()
      ,
      LCL95 = purrr::map(surv, ~ {
        c(
          t[which_first_min_dist_from(.x[["SURV1"]] - 1.96 * .x[["SE1"]], 0.5)],
          t[which_first_min_dist_from(.x[["SURV2"]] - 1.96 * .x[["SE2"]], 0.5)]
        )
      }) |> unlist(),
      UCL95 =  purrr::map(surv, ~ {
        c(
          t[which_first_min_dist_from(.x[["SURV1"]] + 1.96 * .x[["SE1"]], 0.5)],
          t[which_first_min_dist_from(.x[["SURV2"]] + 1.96 * .x[["SE2"]], 0.5)]
        )
      }) |> unlist()
    ) |>
    tibble::as_tibble()

}
