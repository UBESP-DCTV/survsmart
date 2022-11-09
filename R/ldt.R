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
  cat("Estimating for A1 arm... \n")
  est1 <- DTR::sub.LDTestimate(pdata = db[db$X == 0, ], t, L)
  cat("Estimating for A2 arm... \n")
  est2 <- DTR::sub.LDTestimate(pdata = db[db$X == 1, ], t, L)
  cat("Estimating for A3 arm... \n")
  est3 <- DTR::sub.LDTestimate(pdata = db[db$X == 2, ], t, L)

  dtr <- get_xz_combs(db) |>
    purrr::pmap_chr(xz_dtr_labels, db = db)

  records <- get_xz_combs(db) |>
    purrr::pmap_int(xz_n_record, db = db)

  events <- get_xz_combs(db) |>
    purrr::pmap_dbl(xz_n_event, db = db)

  censortime <- get_xz_combs(db) |>
    purrr::pmap(xz_censortimes, db = db) |>
    unlist()

  dtr_censored <- get_xz_combs(db) |>
    purrr::pmap(xz_dtr_labels, db = db, censored = TRUE) |>
    unlist()

  results <- list(
    Call = match.call(),
    DTR = dtr,
    records = records,
    events = events,
    censorDTR = dtr_censored,
    censortime = censortime,

    censorsurv = c(
      apply(
        as.array(
          db$U[
            (db$X == 0 & db$R == 1 & db$Z == 0 & db$delta == 0) |
            (db$X == 0 & db$R == 0 & db$delta == 0)
          ]
        ),
        1,
        function(x) {
          if (x < min(est1$t)) {
            1
          } else {
            est1$SURV1[
              abs(est1$t - x) == min(abs(est1$t - x)[est1$t <= x])
            ]
          }
        }
      ),
      apply(
        as.array(
          db$U[
            (db$X == 0 & db$R == 1 & db$Z == 1 & db$delta == 0) |
            (db$X == 0 & db$R == 0 & db$delta == 0)
          ]
        ),
        1,
        function(x) {
          if (x < min(est1$t)) {
            1
          } else {
            est1$SURV2[
              abs(est1$t - x) == min(abs(est1$t - x)[est1$t <= x])
            ]
          }
        }
      ),
      apply(
        as.array(
          db$U[
            (db$X == 1 & db$R == 1 & db$Z == 0 & db$delta == 0) |
            (db$X == 1 & db$R == 0 & db$delta == 0)
          ]
        ),
        1,
        function(x) {
          if (x < min(est2$t)) {
            1
          } else {
            est2$SURV1[
              abs(est2$t - x) == min(abs(est2$t - x)[est2$t <= x])
            ]
          }
        }
      ),
      apply(
        as.array(
          db$U[
            (db$X == 1 & db$R == 1 & db$Z == 1 & db$delta == 0) |
            (db$X == 1 & db$R == 0 & db$delta == 0)
          ]
        ),
        1,
        function(x) {
          if (x < min(est2$t)) {
            1
          } else {
            est2$SURV2[abs(est2$t - x) == min(abs(est2$t - x)[est2$t <= x])]
          }
        }
      ),
      apply(
        as.array(
          db$U[
            (db$X == 2 & db$R == 1 & db$Z == 0 & db$delta == 0) |
            (db$X == 2 & db$R == 0 & db$delta == 0)
          ]
        ),
        1,
        function(x) {
          if (x < min(est3$t)) {
            1
          } else {
            est3$SURV1[abs(est3$t - x) == min(abs(est3$t - x)[est3$t <= x])]
          }
        }
      ),
      apply(
        as.array(
          db$U[
            (db$X == 2 & db$R == 1 & db$Z == 1 & db$delta == 0) |
            (db$X == 2 & db$R == 0 & db$delta == 0)
          ]
        ),
        1,
        function(x) {
          if (x < min(est3$t)) {
            1
          } else {
            est3$SURV2[abs(est3$t - x) == min(abs(est3$t - x)[est3$t <= x])]
          }
        }
      )
    ),
    time = t, n.risk = n.risk, n.event = n.event,
    SURV11 = est1$SURV1, SURV12 = est1$SURV2,
    SURV21 = est2$SURV1, SURV22 = est2$SURV2,
    SURV31 = est3$SURV1, SURV32 = est3$SURV2,
    SE11 = est1$SE1, SE12 = est1$SE2, COV1112 = est1$COV12,
    SE21 = est2$SE1, SE22 = est2$SE2, COV2122 = est2$COV12,
    SE31 = est3$SE1, SE32 = est3$SE2, COV3132 = est3$COV12
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
