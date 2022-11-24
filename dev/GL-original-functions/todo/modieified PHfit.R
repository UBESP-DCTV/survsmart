function (data, covar = NULL) 
{
  n <- nrow(data)
  X <- data$X
  TR <- data$TR
  R <- data$R
  Z <- data$Z
  U <- data$U
  delta <- data$delta
  if (is.null(X)) 
    stop("X can not be empty")
  if (is.null(TR)) 
    stop("TR can not be empty")
  if (is.null(R)) 
    stop("R can not be empty")
  if (is.null(Z)) 
    stop("Z can not be empty")
  if (is.null(U)) 
    stop("U can not be empty")
  if (is.null(delta)) 
    stop("delta can not be empty")
  if (is.null(covar)) {
    data.model <- data.frame(t.start = c(rep(0, n), TR[which(TR <= U & R == 1)]), 
                             t.end = c(U[which(TR > U | R == 0)], 
                             TR[which(TR <= U & R == 1)], U[which(TR <= U & R == 
                             1)]), t.delta = c(delta[which(TR > U | R == 
                             0)], rep(0, length(which(TR <= U & R == 1))), delta[which(TR <= 
                             U & R == 1)]), 
                             V.X = c(X[which(TR > U | R == 0)], 
                             X[which(TR <= U & R == 1)], X[which(TR <= U & R == 
                             1)]), V.R = c(rep(0, n), rep(1, length(which(TR <= 
                             U & R == 1)))), V.XR = c(rep(0, n), X[which(TR <= 
                             U & R == 1)]), V.RZ = c(rep(0, n), Z[which(TR <= 
                             U & R == 1)]), V.XRZ = c(rep(0, n), X[which(TR <= 
                             U & R == 1)] * Z[which(TR <= U & R == 1)]))
    if (length(which(data.model$t.end == 0)) > 0) 
      data.model <- data.model[which(data.model$t.end != 
                                       0), ]
    names(data.model) <- c("start", "end", "delta", "X", 
                           "R", "XR", "RZ", "XRZ")
    fit <- coxph(Surv(start, end, delta) ~ ., data = data.model)
    fit$call <- "coxph(Surv(U, delta)~ X + R + XR + RZ + XRZ)"
  }
  else {
    if (FALSE %in% (covar %in% names(data))) {
      stop("Covariate(s) can not be found in the data")
    }
    else {
      V <- data[, names(data) %in% covar]
    }
    if (NCOL(V) == 1) {
      data.model <- data.frame(t.start = c(rep(0, n), 
                                           TR[which(TR <= U & R == 1)]), t.end = c(U[which(TR > 
                                           U | R == 0)], TR[which(TR <= U & R == 1)], U[which(TR <= 
                                           U & R == 1)]), t.delta = c(delta[which(TR > 
                                           U | R == 0)], rep(0, length(which(TR <= U & 
                                           R == 1))), delta[which(TR <= U & R == 1)]), 
                               V.X = c(X[which(TR > U | R == 0)], X[which(TR <= 
                                           U & R == 1)], X[which(TR <= U & R == 1)]), 
                               V.R = c(rep(0, n), rep(1, length(which(TR <= 
                                          U & R == 1)))), V.XR = c(rep(0, n), X[which(TR <= 
                                          U & R == 1)]), V.RZ = c(rep(0, n), Z[which(TR <= 
                                          U & R == 1)]), V.XRZ = c(rep(0, n), X[which(TR <= 
                                          U & R == 1)] * Z[which(TR <= U & R == 1)]), 
                               V.V = c(V[which(TR > U | R == 0)], V[which(TR <= 
                                        U & R == 1)], V[which(TR <= U & R == 1)]))
      if (length(which(data.model$t.end == 0)) > 0) 
        data.model <- data.model[which(data.model$t.end != 
                                         0), ]
      names(data.model) <- c("start", "end", "delta", 
                             "X", "R", "XR", "RZ", "XRZ", covar)
      fit <- coxph(Surv(start, end, delta) ~ ., data = data.model)
      fit$call <- paste("coxph(Surv(U, delta) ~ X + R + XR + RZ + XRZ + ", 
                        covar, ")", sep = "")
    }
    if (NCOL(V) > 1) {
      data.model <- data.frame(t.start = c(rep(0, n), 
                                           TR[which(TR <= U & R == 1)]), t.end = c(U[which(TR > 
                                           U | R == 0)], TR[which(TR <= U & R == 1)], U[which(TR <= 
                                           U & R == 1)]), t.delta = c(delta[which(TR > 
                                           U | R == 0)], rep(0, length(which(TR <= U & 
                                           R == 1))), delta[which(TR <= U & R == 1)]), 
                               V.X = c(X[which(TR > U | R == 0)], X[which(TR <= 
                                           U & R == 1)], X[which(TR <= U & R == 1)]), 
                               V.R = c(rep(0, n), rep(1, length(which(TR <= 
                                           U & R == 1)))), V.XR = c(rep(0, n), X[which(TR <= 
                                           U & R == 1)]), V.RZ = c(rep(0, n), Z[which(TR <= 
                                           U & R == 1)]), V.XRZ = c(rep(0, n), X[which(TR <= 
                                           U & R == 1)] * Z[which(TR <= U & R == 1)]), 
                               rbind(V[which(TR > U | R == 0), ], V[which(TR <= 
                                           U & R == 1), ], V[which(TR <= U & R == 1), 
                                                                            ]))
      if (length(which(data.model$t.end == 0)) > 0) 
        data.model <- data.model[which(data.model$t.end != 
                                         0), ]
      names(data.model) <- c("start", "end", "delta", 
                             "X", "R", "XR", "RZ", "XRZ", covar)
      fit <- coxph(Surv(start, end, delta) ~ ., data = data.model)
      fit$call <- paste("coxph(Surv(U, delta) ~ X + R + XR + RZ + XRZ + ", 
                        paste(covar, collapse = " + "), ")", sep = "")
    }
  }
  class(fit) <- "coxph"
  return(fit)
}
