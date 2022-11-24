########## da verificare sub.WRSEestimate, non credo cambi ma meglio vedere----


#' WRSE
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
#' @return ([wrse]) object (inherited from [dtr])
#' @export
wrse <- function(db, L = .Machine$double.xmax) {

  if (is.null(db$X)) {
    stop("X can not be empty")
  }
  if (is.null(db$R)) {
    stop("R can not be empty")
  }
  if (is.null(db$Z)) {
    stop("Z can not be empty")
  }
  if (is.null(db$U)) {
    stop("U can not be empty")
  }
  if (is.null(db$delta)) {
    stop("delta can not be empty")
  }



  t <- unique(db$U[which(db$delta == 1)])
  t <- t[order(t)]
  n.risk <- apply(as.array(t), 1, function(x) {
    sum(as.numeric(db$U >=
      x))
  })
  n.event <- apply(as.array(t), 1, function(x) {
    length(which(db$U ==
      x & db$delta == 1))
  })
  cat("Estimating for A1 arm... \n")
  est1 <- DTR::sub.WRSEestimate(pdata = db[which(db$X == 0), ], t)
  cat("Estimating for A2 arm... \n")
  est2 <- DTR::sub.WRSEestimate(pdata = db[which(db$X == 1), ], t)
  cat("Estimating for A3 arm... \n")
  est3 <- DTR::sub.WRSEestimate(pdata = db[which(db$X == 2), ], t)
  censorDTR <- censortime <- NULL
  results <- list(
    Call = match.call(), DTR = c(
      "A1B1", "A1B2",
      "A2B1", "A2B2",
      "A3B1", "A3B2"
    ),
    records = c(
      length(which((db$X == 0 & db$R == 0) |
        (db$X == 0 & db$R == 1 & db$Z == 0))),
      length(which((db$X == 0 & db$R == 0) |
        (db$X == 0 & db$R == 1 & db$Z == 1))),
      length(which((db$X == 1 & db$R == 0) |
        (db$X == 1 & db$R == 1 & db$Z == 0))),
      length(which((db$X == 1 & db$R == 0) |
        (db$X == 1 & db$R == 1 & db$Z == 1))),
      length(which((db$X == 2 & db$R == 0) |
        (db$X == 2 & db$R == 1 & db$Z == 0))),
      length(which((db$X == 2 & db$R == 0) |
        (db$X == 2 & db$R == 1 & db$Z == 1)))
    ),
    events = c(
      sum(db$delta[which((db$X == 0 & db$R == 0) |
        (db$X == 0 & db$R == 1 & db$Z == 0))]),
      sum(db$delta[which((db$X == 0 & db$R == 0) |
        (db$X == 0 & db$R == 1 & db$Z == 1))]),
      sum(db$delta[which((db$X == 1 & db$R == 0) |
        (db$X == 1 & db$R == 1 & db$Z == 0))]),
      sum(db$delta[which((db$X == 1 & db$R == 0) |
        (db$X == 1 & db$R == 1 & db$Z == 1))]),
      sum(db$delta[which((db$X == 2 & db$R == 0) |
        (db$X == 2 & db$R == 1 & db$Z == 0))]),
      sum(db$delta[which((db$X == 2 & db$R == 0) |
        (db$X == 2 & db$R == 1 & db$Z == 1))])
    ),
    censorDTR = c(
      rep("A1B1", length(which((db$X == 0 &
        db$R == 1 & db$Z == 0 & db$delta == 0) | (db$X ==
        0 & db$R == 0 & db$delta == 0)))),
      rep("A1B2", length(which((db$X == 0 & db$R == 1 & db$Z ==
        1 & db$delta == 0) | (db$X == 0 & db$R ==
        0 & db$delta == 0)))),
      rep("A2B1", length(which((db$X ==
        1 & db$R == 1 & db$Z == 0 & db$delta == 0) |
        (db$X == 1 & db$R == 0 & db$delta == 0)))),
      rep("A2B2", length(which((db$X == 1 & db$R ==
        1 & db$Z == 1 & db$delta == 0) | (db$X ==
        1 & db$R == 0 & db$delta == 0)))),
      rep("A3B1", length(which((db$X == 2 & db$R == 1 & db$Z == 0 & db$delta == 0) |
        (db$X == 2 & db$R == 0 & db$delta == 0)))),
      rep("A3B2", length(which((db$X == 2 & db$R == 1 & db$Z == 1 & db$delta == 0) |
        (db$X == 2 & db$R == 0 & db$delta == 0))))
    ),
    censortime = c(
      db$U[which((db$X == 0 & db$R == 1 & db$Z == 0 & db$delta == 0) |
        (db$X == 0 & db$R == 0 & db$delta == 0))],
      db$U[which((db$X == 0 & db$R == 1 & db$Z == 1 & db$delta == 0) |
        (db$X == 0 & db$R == 0 & db$delta == 0))],
      db$U[which((db$X == 1 & db$R == 1 & db$Z == 0 & db$delta == 0) |
        (db$X == 1 & db$R == 0 & db$delta == 0))],
      db$U[which((db$X == 1 & db$R == 1 & db$Z == 1 & db$delta == 0) |
        (db$X == 1 & db$R == 0 & db$delta == 0))],
      db$U[which((db$X == 2 & db$R == 1 & db$Z == 0 & db$delta == 0) |
        (db$X == 2 & db$R == 0 & db$delta == 0))],
      db$U[which((db$X == 2 & db$R == 1 & db$Z == 1 & db$delta == 0) |
        (db$X == 2 & db$R == 0 & db$delta == 0))]
    ),
    censorsurv = c(
      apply(as.array(db$U[which((db$X == 0 & db$R == 1 & db$Z == 0 & db$delta == 0) |
        (db$X == 0 & db$R == 0 & db$delta == 0))]), 1, function(x) {
        if (x < min(est1$t)) {
          1
        } else {
          est1$SURV1[which(abs(est1$t -
            x) == min(abs(est1$t - x)[which(est1$t <=
            x)]))]
        }
      }),
      apply(as.array(db$U[which((db$X == 0 & db$R ==
        1 & db$Z == 1 & db$delta == 0) | (db$X ==
        0 & db$R == 0 & db$delta == 0))]), 1, function(x) {
        if (x < min(est1$t)) {
          1
        } else {
          est1$SURV2[which(abs(est1$t -
            x) == min(abs(est1$t - x)[which(est1$t <= x)]))]
        }
      }),
      apply(as.array(db$U[which((db$X == 1 & db$R ==
        1 & db$Z == 0 & db$delta == 0) | (db$X ==
        1 & db$R == 0 & db$delta == 0))]), 1, function(x) {
        if (x < min(est2$t)) {
          1
        } else {
          est2$SURV1[which(abs(est2$t -
            x) == min(abs(est2$t - x)[which(est2$t <= x)]))]
        }
      }),
      apply(as.array(db$U[which((db$X == 1 & db$R ==
        1 & db$Z == 1 & db$delta == 0) | (db$X ==
        1 & db$R == 0 & db$delta == 0))]), 1, function(x) {
        if (x < min(est2$t)) {
          1
        } else {
          est2$SURV2[which(abs(est2$t -
            x) == min(abs(est2$t - x)[which(est2$t <= x)]))]
        }
      }),
      apply(as.array(db$U[which((db$X == 2 & db$R ==
        1 & db$Z == 0 & db$delta == 0) | (db$X ==
        2 & db$R == 0 & db$delta == 0))]), 1, function(x) {
        if (x < min(est3$t)) {
          1
        } else {
          est3$SURV1[which(abs(est3$t -
            x) == min(abs(est3$t - x)[which(est3$t <= x)]))]
        }
      }),
      apply(as.array(db$U[which((db$X == 2 & db$R ==
        1 & db$Z == 1 & db$delta == 0) | (db$X ==
        2 & db$R == 0 & db$delta == 0))]), 1, function(x) {
        if (x < min(est3$t)) {
          1
        } else {
          est3$SURV2[which(abs(est3$t -
            x) == min(abs(est3$t - x)[which(est3$t <= x)]))]
        }
      })
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
