########## da verificare sub.WRSEestimate, non credo cambi ma meglio vedere----

WRSEestimate_gl <- function(data) {
  if (is.null(data$X)) {
    stop("X can not be empty")
  }
  if (is.null(data$TR)) {
    stop("TR can not be empty")
  }
  if (is.null(data$R)) {
    stop("R can not be empty")
  }
  if (is.null(data$Z)) {
    stop("Z can not be empty")
  }
  if (is.null(data$U)) {
    stop("U can not be empty")
  }
  if (is.null(data$delta)) {
    stop("delta can not be empty")
  }
  t <- unique(data$U[which(data$delta == 1)])
  t <- t[order(t)]
  n.risk <- apply(as.array(t), 1, function(x) {
    sum(as.numeric(data$U >=
      x))
  })
  n.event <- apply(as.array(t), 1, function(x) {
    length(which(data$U ==
      x & data$delta == 1))
  })
  cat("Estimating for A1 arm... \n")
  est1 <- sub.WRSEestimate(pdata = data[which(data$X == 0), ], t)
  cat("Estimating for A2 arm... \n")
  est2 <- sub.WRSEestimate(pdata = data[which(data$X == 1), ], t)
  cat("Estimating for A3 arm... \n")
  est3 <- sub.WRSEestimate(pdata = data[which(data$X == 2), ], t)
  censorDTR <- censortime <- NULL
  results <- list(
    Call = match.call(), DTR = c(
      "A1B1", "A1B2",
      "A2B1", "A2B2",
      "A3B1", "A3B2"
    ),
    records = c(
      length(which((data$X == 0 & data$R == 0) |
        (data$X == 0 & data$R == 1 & data$Z == 0))),
      length(which((data$X == 0 & data$R == 0) |
        (data$X == 0 & data$R == 1 & data$Z == 1))),
      length(which((data$X == 1 & data$R == 0) |
        (data$X == 1 & data$R == 1 & data$Z == 0))),
      length(which((data$X == 1 & data$R == 0) |
        (data$X == 1 & data$R == 1 & data$Z == 1))),
      length(which((data$X == 2 & data$R == 0) |
        (data$X == 2 & data$R == 1 & data$Z == 0))),
      length(which((data$X == 2 & data$R == 0) |
        (data$X == 2 & data$R == 1 & data$Z == 1)))
    ),
    events = c(
      sum(data$delta[which((data$X == 0 & data$R == 0) |
        (data$X == 0 & data$R == 1 & data$Z == 0))]),
      sum(data$delta[which((data$X == 0 & data$R == 0) |
        (data$X == 0 & data$R == 1 & data$Z == 1))]),
      sum(data$delta[which((data$X == 1 & data$R == 0) |
        (data$X == 1 & data$R == 1 & data$Z == 0))]),
      sum(data$delta[which((data$X == 1 & data$R == 0) |
        (data$X == 1 & data$R == 1 & data$Z == 1))]),
      sum(data$delta[which((data$X == 2 & data$R == 0) |
        (data$X == 2 & data$R == 1 & data$Z == 0))]),
      sum(data$delta[which((data$X == 2 & data$R == 0) |
        (data$X == 2 & data$R == 1 & data$Z == 1))])
    ),
    censorDTR = c(
      rep("A1B1", length(which((data$X == 0 &
        data$R == 1 & data$Z == 0 & data$delta == 0) | (data$X ==
        0 & data$R == 0 & data$delta == 0)))),
      rep("A1B2", length(which((data$X == 0 & data$R == 1 & data$Z ==
        1 & data$delta == 0) | (data$X == 0 & data$R ==
        0 & data$delta == 0)))),
      rep("A2B1", length(which((data$X ==
        1 & data$R == 1 & data$Z == 0 & data$delta == 0) |
        (data$X == 1 & data$R == 0 & data$delta == 0)))),
      rep("A2B2", length(which((data$X == 1 & data$R ==
        1 & data$Z == 1 & data$delta == 0) | (data$X ==
        1 & data$R == 0 & data$delta == 0)))),
      rep("A3B1", length(which((data$X == 2 & data$R == 1 & data$Z == 0 & data$delta == 0) |
        (data$X == 2 & data$R == 0 & data$delta == 0)))),
      rep("A3B2", length(which((data$X == 2 & data$R == 1 & data$Z == 1 & data$delta == 0) |
        (data$X == 2 & data$R == 0 & data$delta == 0))))
    ),
    censortime = c(
      data$U[which((data$X == 0 & data$R == 1 & data$Z == 0 & data$delta == 0) |
        (data$X == 0 & data$R == 0 & data$delta == 0))],
      data$U[which((data$X == 0 & data$R == 1 & data$Z == 1 & data$delta == 0) |
        (data$X == 0 & data$R == 0 & data$delta == 0))],
      data$U[which((data$X == 1 & data$R == 1 & data$Z == 0 & data$delta == 0) |
        (data$X == 1 & data$R == 0 & data$delta == 0))],
      data$U[which((data$X == 1 & data$R == 1 & data$Z == 1 & data$delta == 0) |
        (data$X == 1 & data$R == 0 & data$delta == 0))],
      data$U[which((data$X == 2 & data$R == 1 & data$Z == 0 & data$delta == 0) |
        (data$X == 2 & data$R == 0 & data$delta == 0))],
      data$U[which((data$X == 2 & data$R == 1 & data$Z == 1 & data$delta == 0) |
        (data$X == 2 & data$R == 0 & data$delta == 0))]
    ),
    censorsurv = c(
      apply(as.array(data$U[which((data$X == 0 & data$R == 1 & data$Z == 0 & data$delta == 0) |
        (data$X == 0 & data$R == 0 & data$delta == 0))]), 1, function(x) {
        if (x < min(est1$t)) {
          1
        } else {
          est1$SURV1[which(abs(est1$t -
            x) == min(abs(est1$t - x)[which(est1$t <=
            x)]))]
        }
      }),
      apply(as.array(data$U[which((data$X == 0 & data$R ==
        1 & data$Z == 1 & data$delta == 0) | (data$X ==
        0 & data$R == 0 & data$delta == 0))]), 1, function(x) {
        if (x < min(est1$t)) {
          1
        } else {
          est1$SURV2[which(abs(est1$t -
            x) == min(abs(est1$t - x)[which(est1$t <= x)]))]
        }
      }),
      apply(as.array(data$U[which((data$X == 1 & data$R ==
        1 & data$Z == 0 & data$delta == 0) | (data$X ==
        1 & data$R == 0 & data$delta == 0))]), 1, function(x) {
        if (x < min(est2$t)) {
          1
        } else {
          est2$SURV1[which(abs(est2$t -
            x) == min(abs(est2$t - x)[which(est2$t <= x)]))]
        }
      }),
      apply(as.array(data$U[which((data$X == 1 & data$R ==
        1 & data$Z == 1 & data$delta == 0) | (data$X ==
        1 & data$R == 0 & data$delta == 0))]), 1, function(x) {
        if (x < min(est2$t)) {
          1
        } else {
          est2$SURV2[which(abs(est2$t -
            x) == min(abs(est2$t - x)[which(est2$t <= x)]))]
        }
      }),
      apply(as.array(data$U[which((data$X == 2 & data$R ==
        1 & data$Z == 0 & data$delta == 0) | (data$X ==
        2 & data$R == 0 & data$delta == 0))]), 1, function(x) {
        if (x < min(est3$t)) {
          1
        } else {
          est3$SURV1[which(abs(est3$t -
            x) == min(abs(est3$t - x)[which(est3$t <= x)]))]
        }
      }),
      apply(as.array(data$U[which((data$X == 2 & data$R ==
        1 & data$Z == 1 & data$delta == 0) | (data$X ==
        2 & data$R == 0 & data$delta == 0))]), 1, function(x) {
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
  return(results)
}

print.DTR_gl <- function(x, ...) {
  if (!is.null(x$Call)) {
    cat("Call: ")
    dput(x$Call)
    cat("\n")
  }

  temp <- data.frame(
    DTR = x$DTR,
    records = x$records,
    events = x$events,
    median = c(
      min(x$time[which(abs(x$SURV11 - 0.5) == min(abs(x$SURV11 -
        0.5)[which(x$SURV11 <= 0.5)]))]), min(x$time[which(abs(x$SURV12 -
        0.5) == min(abs(x$SURV12 - 0.5)[which(x$SURV12 <=
        0.5)]))]),
      min(x$time[which(abs(x$SURV21 - 0.5) ==
        min(abs(x$SURV21 - 0.5)[which(x$SURV21 <= 0.5)]))]),
      min(x$time[which(abs(x$SURV22 - 0.5) == min(abs(x$SURV22 -
        0.5)[which(x$SURV22 <= 0.5)]))]),
      min(x$time[which(abs(x$SURV31 - 0.5) ==
        min(abs(x$SURV31 - 0.5)[which(x$SURV31 <= 0.5)]))]),
      min(x$time[which(abs(x$SURV32 - 0.5) == min(abs(x$SURV32 -
        0.5)[which(x$SURV32 <= 0.5)]))])
    ),
    LCL95 = c(
      min(x$time[which(abs(x$SURV11 -
        1.96 * x$SE11 - 0.5) == min(abs(x$SURV11 - 1.96 *
        x$SE11 - 0.5)[which((x$SURV11 - 1.96 * x$SE11) <=
        0.5)]))]), min(x$time[which(abs(x$SURV12 - 1.96 *
        x$SE12 - 0.5) == min(abs(x$SURV12 - 1.96 * x$SE12 -
        0.5)[which((x$SURV12 - 1.96 * x$SE12) <= 0.5)]))]),
      min(x$time[which(abs(x$SURV21 - 1.96 * x$SE21 -
        0.5) == min(abs(x$SURV21 - 1.96 * x$SE21 - 0.5)[which((x$SURV21 -
        1.96 * x$SE21) <= 0.5)]))]), min(x$time[which(abs(x$SURV22 -
        1.96 * x$SE22 - 0.5) == min(abs(x$SURV22 - 1.96 *
        x$SE22 - 0.5)[which((x$SURV22 - 1.96 * x$SE22) <=
        0.5)]))]),
      min(x$time[which(abs(x$SURV31 - 1.96 * x$SE31 -
        0.5) == min(abs(x$SURV31 - 1.96 * x$SE31 - 0.5)[which((x$SURV31 -
        1.96 * x$SE31) <= 0.5)]))]), min(x$time[which(abs(x$SURV32 -
        1.96 * x$SE32 - 0.5) == min(abs(x$SURV32 - 1.96 *
        x$SE32 - 0.5)[which((x$SURV32 - 1.96 * x$SE32) <=
        0.5)]))])
    ),
    UCL95 = c(
      min(x$time[which(abs(x$SURV11 +
        1.96 * x$SE11 - 0.5) == min(abs(x$SURV11 + 1.96 *
        x$SE11 - 0.5)[which((x$SURV11 + 1.96 * x$SE11) <=
        0.5)]))]), min(x$time[which(abs(x$SURV12 + 1.96 *
        x$SE12 - 0.5) == min(abs(x$SURV12 + 1.96 * x$SE12 -
        0.5)[which((x$SURV12 + 1.96 * x$SE12) <= 0.5)]))]),
      min(x$time[which(abs(x$SURV21 + 1.96 * x$SE21 -
        0.5) == min(abs(x$SURV21 + 1.96 * x$SE21 - 0.5)[which((x$SURV21 +
        1.96 * x$SE21) <= 0.5)]))]), min(x$time[which(abs(x$SURV22 +
        1.96 * x$SE22 - 0.5) == min(abs(x$SURV22 + 1.96 *
        x$SE22 - 0.5)[which((x$SURV22 + 1.96 * x$SE22) <=
        0.5)]))]),
      min(x$time[which(abs(x$SURV31 + 1.96 * x$SE31 -
        0.5) == min(abs(x$SURV31 + 1.96 * x$SE31 - 0.5)[which((x$SURV31 +
        1.96 * x$SE31) <= 0.5)]))]), min(x$time[which(abs(x$SURV32 +
        1.96 * x$SE32 - 0.5) == min(abs(x$SURV32 + 1.96 *
        x$SE32 - 0.5)[which((x$SURV32 + 1.96 * x$SE32) <=
        0.5)]))])
    )
  )
  print(temp, row.names = FALSE)
}
