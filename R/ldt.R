#' LDT
#'
#' @param db (data.frame) with column `X` corresponding to treatment
#'   arms in the first stage (X = 1 for arm A1, X = 2 for arm A2, ... X
#'   = <n> for arm A<n>), `R` for respondent or not in the first arm (R
#'   = 0 for not respondent, R = 1 for respondent), `Z` for treatment
#'   arms for the second stage (Z = 1 for first arm B1, Z = 2 for second
#'   arm B2), `U` for observed time survival (event or censoring time),
#'   `delta`event indicator (delta = 0 for censored, delta = 1 for
#'   events).
#' @param L (dbl, .Machine$double.xmax) restricted survival time.
#'
#' @return ([ldt]) object (inherited from [dtr])
#' @export
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

  map_xz_combs_to <- function(.f, ...) {
    purrr::pmap(xz_combs, .f, db = db, ...) |> unlist()
  }

  structure(
    tibble::tibble(
      dtr = purrr::pmap_chr(xz_combs, xz_dtr_labels, db = db),
      records = purrr::pmap_int(xz_combs, xz_n_record, db = db),
      events = purrr::pmap_dbl(xz_combs, xz_n_event, db = db)
    ),
    call = match.call(),
    censor = tibble::tibble(
      dtr = map_xz_combs_to(xz_dtr_labels, censored = TRUE),
      time = map_xz_combs_to(xz_censortimes),
      surv = map_xz_combs_to(eval_censorsurv, est = est)
    ),
    risk_table = tibble::tibble(
      time = t, n.risk = n.risk, n.event = n.event
    ),
    estimate = est,
    class = c("ldt", "dtr", "tbl_df", "tbl", "data.frame")
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
  label <- glue::glue("A{x}B{z}")

  if (censored) {
    label <- rep(label, sum(are_xz(db, x, z, censored)))
  }
  label
}

eval_censorsurv <- function(est, db, x, z) {
  est_x <- est[[as.character(x)]]
  est_xt <- est_x$t
  est_xz <- est_x[[glue::glue("SURV{z}")]]

  xz_censortimes(db, x, z) |>
    purrr::map_dbl(~ {
      if (.x < min(est_xt)) {
        1
      } else {
        est_xz[which_first_min_dist_from(est_xt, .x)]
      }
    })
}


ldt_x <- function(db, x, t, L) {
  pdata <- dplyr::filter(db, .data[["X"]] == x)

  res <- fit_ldt(pdata = pdata, t, L)
  usethis::ui_done("LDT for A{x} arm estimated.")
  res
}


fit_ldt <- function(pdata, t, L) {
  n <- nrow(pdata)
  R <- pdata$R
  Z <- pdata$Z
  U <- pdata$U
  delta <- pdata$delta
  cens <- 1 - delta

  pi.z <- sum(R * (Z - 1)) / sum(R)
  Q1 <- (1 - R) + R * (2 - Z) / (1 - pi.z)
  Q2 <- (1 - R) + R * (Z - 1) / (pi.z)

  cfit <- summary(survival::survfit(survival::Surv(U, cens) ~ 1))

  K <- purrr::map_dbl(U, ~{
      if (round(.x, 4) < round(min(cfit$time), 4)) {
        1
      } else {
        dt <- round(cfit$time, 4) - round(.x, 4)
        cfit$surv[which.max(dt[dt <= 0])]
      }
    })

  k_nonzero <- K != 0
  delta_k_nonzero <- delta[k_nonzero]
  q1_k_nonzero <- Q1[k_nonzero]
  q2_k_nonzero <- Q2[k_nonzero]
  kk_nonzero <- K[k_nonzero]
  delta_kk_nonzero <- delta_k_nonzero / kk_nonzero

  w1 <- rep(0, n)
  w2 <- rep(0, n)
  w1[k_nonzero] <- delta_kk_nonzero * q1_k_nonzero
  w2[k_nonzero] <- delta_kk_nonzero * q2_k_nonzero

  s <- purrr::map_dbl(U, ~ {
    sind <- as.numeric(U <= .x)
    1 -
      sum(delta_kk_nonzero * sind[k_nonzero]) /
      sum(delta_kk_nonzero)
  })

  SURV1 <- rep(NA_real_, length(t))
  SURV2 <- rep(NA_real_, length(t))
  SE1 <- rep(NA_real_)
  SE2 <- rep(NA_real_, length(t))
  COV12 <- rep(NA_real_, length(t))

  for (j in seq_along(t)) {
    ind <- as.numeric(U <= t[j])
    ind_k_nonzero <- ind[k_nonzero]

    SURV1[[j]] <- 1 - sum(w1 * ind) / sum(w1)
    SURV2[[j]] <- 1 - sum(w2 * ind) / sum(w2)

    ind_k_nonzero_plus_surv1_j_minus_1 <- (ind_k_nonzero - 1 + SURV1[j])
    ind_k_nonzero_plus_surv2_j_minus_1 <- (ind_k_nonzero - 1 + SURV2[j])

    G1 <- rep(0, n)
    G2 <- rep(0, n)
    E1 <- rep(0, n)
    E2 <- rep(0, n)
    E12 <- rep(0, n)
    Y <- rep(0, n)

    for (k in seq_len(n)) {
      pind <- as.numeric(U >= U[k])
      pind_k_nonzero <- pind[k_nonzero]
      delta_pind_k_nonzero <- delta_kk_nonzero * pind_k_nonzero / n

      q1_times_ind_k_nonzero_plus_surv1_j_minus_1 <- q1_k_nonzero *
        ind_k_nonzero_plus_surv1_j_minus_1
      q2_times_ind_k_nonzero_plus_surv2_j_minus_1 <- q2_k_nonzero *
        ind_k_nonzero_plus_surv2_j_minus_1

      if (s[k] != 0) {
        delta_pind_k_nonzero_sk <- delta_pind_k_nonzero / s[k]

        G1[[k]] <- sum(
          delta_pind_k_nonzero_sk *
            q1_times_ind_k_nonzero_plus_surv1_j_minus_1
        )

        G2[[k]] <- sum(
          delta_pind_k_nonzero_sk *
            q2_times_ind_k_nonzero_plus_surv2_j_minus_1
        )
      }

      to_square_surv1 <- (
        q1_times_ind_k_nonzero_plus_surv1_j_minus_1 - G1[k]
      )
      to_square_surv2 <- (
        q2_times_ind_k_nonzero_plus_surv2_j_minus_1 - G2[k]
      )

      E1[[k]] <- sum(delta_pind_k_nonzero * to_square_surv1^2)
      E2[[k]] <- sum(delta_pind_k_nonzero * to_square_surv2^2)

      E12[[k]] <- sum(
        delta_pind_k_nonzero * to_square_surv1 * to_square_surv2
      )

      Y[[k]] <- sum(pind)
    }

    v1 <- sum(
        delta_kk_nonzero *
          q1_k_nonzero^2 *
          ind_k_nonzero_plus_surv1_j_minus_1^2
      ) /
      (n^2) +
      sum(
        E1[k_nonzero] *
          (1 - delta_k_nonzero) *
         (U[k_nonzero] <= L) /
          (kk_nonzero * Y[k_nonzero])
      ) / n

    SE1[[j]] <- sqrt(v1)

    v2 <- sum(
        delta_kk_nonzero *
          q2_k_nonzero^2 *
          ind_k_nonzero_plus_surv2_j_minus_1^2
      ) /
      (n^2) +
      sum(
        E2[k_nonzero] *
          (1 - delta_k_nonzero) *
          (U[k_nonzero] <= L) /
          (kk_nonzero * Y[k_nonzero])
      ) / n

    SE2[[j]] <- sqrt(v2)

    COV12[[j]] <- sum(
        delta_kk_nonzero *
          q1_k_nonzero *
          q2_k_nonzero *
          ind_k_nonzero_plus_surv1_j_minus_1 *
          ind_k_nonzero_plus_surv2_j_minus_1
      ) /
      (n^2) +
      sum(
        E12[k_nonzero] *
          (1 - delta_k_nonzero) *
          (U[k_nonzero] <= L) /
          (kk_nonzero * Y[k_nonzero])
      ) / n
  }

  data.frame(t, SURV1, SURV2, SE1, SE2, COV12)
}
