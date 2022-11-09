ldt <- function(db, L = .Machine$double.xmax) {
  check_dtr_db(db)
  checkmate::qassert(L, "N1(0,)")

  t <- sort(unique(db$U[db$delta == 1]))
  n.risk <- purrr::map_int(t, ~ sum(db$U >= .x))
  n.event <- purrr::map_int(t, ~ sum(db$U == .x & db$delta == 1))

  xz_combs <- get_xz_combs(db)

  est <- unique(xz_combs[["x"]]) |>
    purrr::set_names() |>
    purrr::map(ldt_x, db = db, t = t, L = L)

  structure(
    list(
      Call = match.call(),
      DTR = purrr::pmap_chr(xz_combs, xz_dtr_labels, db = db),
      records = purrr::pmap_int(xz_combs, xz_n_record, db = db),
      events = purrr::pmap_dbl(xz_combs, xz_n_event, db = db),
      censorDTR = xz_combs |>
        purrr::pmap(xz_dtr_labels, db = db, censored = TRUE) |>
        unlist(),
      censortime = purrr::pmap(xz_combs, xz_censortimes, db = db) |>
        unlist(),
      censorsurv = xz_combs |>
        purrr::pmap(eval_censorsurv, est = est, db = db) |>
        purrr::flatten(), # two identical results are returned together

      time = t, n.risk = n.risk, n.event = n.event,
      SURV11 = est[["0"]][["SURV1"]], SURV12 = est[["0"]][["SURV2"]],
      SURV21 = est[["1"]][["SURV1"]], SURV22 = est[["1"]][["SURV2"]],
      SURV31 = est[["2"]][["SURV1"]], SURV32 = est[["2"]][["SURV2"]],
      SE11 = est[["0"]]$SE1, SE12 = est[["0"]]$SE2, COV1112 = est[["0"]]$COV12,
      SE21 = est[["1"]]$SE1, SE22 = est[["1"]]$SE2, COV2122 = est[["1"]]$COV12,
      SE31 = est[["2"]]$SE1, SE32 = est[["2"]]$SE2, COV3132 = est[["2"]]$COV12
    ),
    class = "DTR_gl"
  )
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
    purrr::map(~ {
      delta <- abs(est_xt - .x)
      if (.x < min(est_xt)) {
        1
      } else {
        # can return more than one (identical?) values!!
        est_xz[delta == min(delta[est_xt <= .x])]
      }
    })
}


ldt_x <- function(db, x, t, L) {
  pdata <- dplyr::filter(db, .data[["X"]] == x)

  usethis::ui_todo("Estimating LDT for A{x + 1} arm...")
  res <- fit_ldt(pdata = pdata, t, L)
  usethis::ui_done("LDT for A{x + 1} arm estimated.")
  res
}


fit_ldt <- function(pdata, t, L) {
  n <- nrow(pdata)
  R <- pdata$R
  Z <- pdata$Z
  U <- pdata$U
  delta <- pdata$delta
  cens <- 1 - delta

  pi.z <- sum(R * Z) / sum(R)
  Q1 <- (1 - R) + R * (1 - Z) / (1 - pi.z)
  Q2 <- (1 - R) + R * Z / (pi.z)

  cfit <- summary(survival::survfit(survival::Surv(U, cens) ~ 1))

  K <- rep(0, n)
  for (i in 1:n) {
    if (round(U[i], 4) < round(min(cfit$time), 4)) {
      K[i] <- 1
    } else {
      dt <- round(cfit$time, 4) - round(U[i], 4)
      K[i] <- cfit$surv[which(dt == max(dt[dt <= 0]))[1]]
    }
  }
  w1 <- rep(0, n)
  w2 <- rep(0, n)
  w1[K != 0] <- delta[K != 0] * Q1[K != 0] / K[K != 0]
  w2[K != 0] <- delta[K != 0] * Q2[K != 0] / K[K != 0]

  s <- rep(0, n)
  for (i in 1:n) {
    sind <- as.numeric(U <= U[i])
    s[i] <- 1 - sum(delta[K != 0] * sind[K != 0] / K[K != 0]) /
      sum(delta[K != 0] / K[K != 0])
  }

  SURV1 <- rep(NA, length(t))
  SURV2 <- rep(NA, length(t))
  SE1 <- rep(NA, length(t))
  SE2 <- rep(NA, length(t))
  COV12 <- rep(NA, length(t))
  for (j in 1:length(t)) {
    ind <- as.numeric(U <= t[j])
    SURV1[j] <- 1 - sum(w1 * ind) / sum(w1)
    SURV2[j] <- 1 - sum(w2 * ind) / sum(w2)
    G1 <- G2 <- rep(0, n)
    E1 <- E2 <- E12 <- rep(0, n)
    Y <- rep(0, n)
    for (k in 1:n) {
      pind <- as.numeric(U >= U[k])
      if (s[k] != 0) {
        G1[k] <- sum(
          delta[K != 0] *
          Q1[K != 0] *
          (ind[K != 0] - 1 + SURV1[j]) *
          pind[K != 0] /
          K[K != 0]
        ) / (n * s[k])
      }
      if (s[k] != 0) {
        G2[k] <- sum(
          delta[K != 0] *
          Q2[K !=  0] *
          (ind[K != 0] - 1 + SURV2[j]) *
          pind[K != 0] /
          K[K != 0]
        ) / (n * s[k])
      }
      E1[k] <- sum(
        delta[K != 0] *
        (Q1[K != 0] * (ind[K != 0] - 1 + SURV1[j]) - G1[k])^2 *
        pind[K != 0] /
        K[K != 0]
      ) / n
      E2[k] <- sum(
        delta[K != 0] *
        (Q2[K != 0] * (ind[K != 0] - 1 + SURV2[j]) - G2[k])^2 *
        pind[K != 0] /
        K[K != 0]
      ) / n
      E12[k] <- sum(
        delta[K != 0] *
        (Q1[K != 0] * (ind[K != 0] - 1 + SURV1[j]) - G1[k]) *
        (Q2[K != 0] * (ind[K != 0] - 1 + SURV2[j]) - G2[k]) *
        pind[K != 0] /
        K[K != 0]
      ) / n
      Y[k] <- sum(pind)
    }

    v1 <- sum(
        delta[K != 0] *
        Q1[K != 0]^2 *
        (ind[K != 0] - 1 + SURV1[j])^2 /
        K[K != 0]
      ) /
      (n^2) +
      sum(
        E1[K != 0] *
        (1 - delta[K != 0]) *
        (U[K != 0] <= L) /
        (K[K != 0] * Y[K != 0])
      ) / n
    SE1[j] <- sqrt(v1)
    v2 <- sum(
        delta[K != 0] *
        Q2[K != 0]^2 *
        (ind[K != 0] - 1 + SURV2[j])^2 /
        K[K != 0]
      ) /
      (n^2) +
      sum(
        E2[K != 0] *
        (1 - delta[K != 0]) *
        (U[K != 0] <= L) /
        (K[K != 0] * Y[K != 0])
      ) / n
    SE2[j] <- sqrt(v2)
    COV12[j] <- sum(
        delta[K != 0] *
        Q1[K != 0] *
        Q2[K != 0] *
        (ind[K != 0] - 1 + SURV1[j]) *
        (ind[K != 0] - 1 + SURV2[j]) /
        K[K !=  0]
      ) /
      (n^2) +
      sum(
        E12[K != 0] *
          (1 - delta[K !=  0]) *
          (U[K != 0] <= L) /
          (K[K != 0] * Y[K != 0])
      ) / n
  }

  data.frame(t, SURV1, SURV2, SE1, SE2, COV12)
}
