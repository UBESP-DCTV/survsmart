contrast_wald_gl<-function (est, t = quantile(est$time, 0.75)) 
{
  if (t >= max(est$time)) 
    stop("Time beyond the scope of the data")
  if (t <= min(est$time)) 
    stop("Time beyond the scope of the data")
  index <- which(abs(est$time - t) == min(abs(est$time - t)[which(est$time <= 
                                                                    t)]))
  SURV <- matrix(c(est$SURV11[index], 
                   est$SURV12[index], 
                   est$SURV21[index], 
                   est$SURV22[index],
                   est$SURV31[index],
                   est$SURV32[index]), 
                 nrow = 6, ncol = 1)
  VAR <- matrix(c(est$SE11[index]^2, 
                  est$COV1112[index], 0, 0, 0, 0,
                  est$COV1112[index], 
                  est$SE12[index]^2, 0, 0, 0, 0, 0, 0,
                  est$SE21[index]^2, 
                  est$COV2122[index], 0, 0, 0, 0,
                  est$COV2122[index], 
                  est$SE22[index]^2, 0, 0, 0, 0, 0, 0,
                  est$SE31[index]^2,
                  est$COV3132[index], 0, 0, 0, 0,
                  est$COV3132[index],
                  est$SE32[index]^2
                  ), 
                  nrow = 6, ncol = 6)
  ##ferma qui---
  test_overall <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(1, 1, 1, 1, 1, -1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, -1), nrow = 5, ncol = 6))
  test_1112 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(1, -1, 0, 0, 0, 0), nrow = 1, ncol = 6))
  test_1121 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(1, 0, -1, 0, 0, 0), nrow = 1, ncol = 6))
  test_1122 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(1, 0, 0, -1, 0, 0), nrow = 1, ncol = 6))
  test_1221 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 1, -1, 0, 0, 0), nrow = 1, ncol = 6))
  test_1222 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 1, 0, -1, 0, 0), nrow = 1, ncol = 6))
  test_2122 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 0, 1, -1, 0, 0), nrow = 1, ncol = 6))
  #new
  test_1131 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(1, 0, 0, 0, -1, 0), nrow = 1, ncol = 6))
  test_1132 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(1, 0, 0, 0, 0, -1), nrow = 1, ncol = 6))
  test_1231 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 1, 0, 0, -1, 0), nrow = 1, ncol = 6))
  test_1232 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 1, 0, 0, 0, -1), nrow = 1, ncol = 6))
  test_2131 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 0, 1, 0, -1, 0), nrow = 1, ncol = 6))
  test_2132 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 0, 1, 0, 0, -1), nrow = 1, ncol = 6))
  test_2231 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 0, 0, 1, -1, 0), nrow = 1, ncol = 6))
  test_2232 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 0, 0, 1, 0, -1), nrow = 1, ncol = 6))
  test_3132 <- wald.test(b = SURV, Sigma = VAR, L = matrix(c(0, 0, 0, 0, 1, -1), nrow = 1, ncol = 6))
  results <- t(data.frame(test_overall$result, test_1112$result, 
                          test_1121$result, test_1122$result, test_1221$result, 
                          test_1222$result, test_2122$result,
                          test_1131$result, test_1132$result,
                          test_1231$result, test_1232$result,
                          test_2131$result, test_2132$result,
                          test_2231$result, test_2232$result,
                          test_3132$result))
  rownames(results) <- NULL
  TEST <- data.frame(c("A1B1=A1B2=A2B1=A2B2=A3B1=A3B2", "A1B1=A1B2", 
                       "A1B1=A2B1", "A1B1=A2B2", "A1B2=A2B1", "A1B2=A2B2", 
                       "A2B1=A2B2", "A1B1=A3B1", "A1B1=A3B2",
                       "A1B2=A3B1", "A1B2=A3B2",
                       "A2B1=A3B1", "A2B1=A3B2",
                       "A2B2=A3B1", "A2B2=A3B2",
                       "A3B1=A3B2"), results)
  names(TEST) <- c(paste("H0 (t=", round(t, 2), ")", sep = ""), 
                   "test statistic", "df", "p")
  return(TEST)
}