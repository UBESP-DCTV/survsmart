ldt <- function(db, L = .Machine$double.xmax) {
  check_dtr_db(db)
  checkmate::qassert(L, "N1(0,)")

  t <- unique(db$U[db$delta == 1])
  t <- t[order(t)]
  n.risk <- apply(as.array(t), 1, function(x) {
    sum(as.numeric(db$U >=
                     x))
  })
  n.event <- apply(as.array(t), 1, function(x) {
    sum(db$U ==
                   x & db$delta == 1)
  })

  xz_combs <- get_xz_combs(db)


  est <- unique(xz_combs[["x"]]) |>
    purrr::set_names() |>
    purrr::map(~{
      usethis::ui_todo("Estimating LDT for A{.x + 1} arm...")
      res <- DTR::sub.LDTestimate(pdata = db[db[["X"]] == .x, ], t, L)
      usethis::ui_done("LDT for A{.x + 1} arm estimated.")
      res
    })

  dtr <- purrr::pmap_chr(xz_combs, xz_dtr_labels, db = db)
  records <- purrr::pmap_int(xz_combs, xz_n_record, db = db)
  events <- purrr::pmap_dbl(xz_combs, xz_n_event, db = db)
  censortime <- purrr::pmap(xz_combs, xz_censortimes, db = db) |>
    unlist()
  dtr_censored <- xz_combs |>
    purrr::pmap(xz_dtr_labels, db = db, censored = TRUE) |>
    unlist()
  censorsurv <- xz_combs |>
    purrr::pmap(eval_censorsurv, est = est, db = db) |>
    purrr::flatten() # two identical results are returned together


  results <- list(
    Call = match.call(),
    DTR = dtr,
    records = records,
    events = events,
    censorDTR = dtr_censored,
    censortime = censortime,
    censorsurv = censorsurv,

    time = t, n.risk = n.risk, n.event = n.event,
    SURV11 = est[["0"]][["SURV1"]], SURV12 = est[["0"]][["SURV2"]],
    SURV21 = est[["1"]][["SURV1"]], SURV22 = est[["1"]][["SURV2"]],
    SURV31 = est[["2"]][["SURV1"]], SURV32 = est[["2"]][["SURV2"]],
    SE11 = est[["0"]]$SE1, SE12 = est[["0"]]$SE2, COV1112 = est[["0"]]$COV12,
    SE21 = est[["1"]]$SE1, SE22 = est[["1"]]$SE2, COV2122 = est[["1"]]$COV12,
    SE31 = est[["2"]]$SE1, SE32 = est[["2"]]$SE2, COV3132 = est[["2"]]$COV12
  )

  class(results) <- "DTR_gl"
  results
}


get_xz_combs <- function(db) {
  list(x = unique(db[["X"]]), z = unique(db[["Z"]])) |>
    purrr::cross_df() |>
    dplyr::arrange(.data[["x"]], .data[["z"]])
}


are_xz <- function(db, x, z, censored = FALSE) {
  are_x <- db[["X"]] == x
  are_z <- db[["Z"]] == z

  if (censored) {
    are_x <- are_x & db[["delta"]] == 0
  }

  (are_x & db[["R"]] == 0) | (are_x & db[["R"]] == 1 & are_z)
}


xz_n_record <- function(db, x, z) {
  sum(are_xz(db = db, x = x, z = z))
}


xz_n_event <- function(db, x, z) {
  sum(db$delta[are_xz(db = db, x = x, z = z)])
}

xz_censortimes <- function(db, x, z) {
  db[["U"]][are_xz(db = db, x = x, z = z, censored = TRUE)]
}

xz_dtr_labels <- function(db, x, z, censored = FALSE) {
  label <- glue::glue("A{x + 1}B{z + 1}")

  if (censored) {
    label <- rep(label, sum(are_xz(db, x, z, censored)))
  }
  label
}

eval_censorsurv <- function(est, db, x, z) {
  est_x <- est[[as.character(x)]]
  est_xt <- est_x$t
  est_xz <- est_x[[glue::glue("SURV{z + 1}")]]

  xz_censortimes(db, x, z) |>
    purrr::map(~{
      delta <- abs(est_xt - .x)
      if (.x < min(est_xt)) {
        1
      } else {
        # can return more than one (identical?) values!!
        est_xz[delta == min(delta[est_xt <= .x])]
      }
    })
}
