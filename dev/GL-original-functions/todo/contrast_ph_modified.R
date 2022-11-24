function (fit) 
{
  EST <- matrix(as.numeric(coef(fit)[1:5]), nrow = 5, ncol = 1)
  VAR <- vcov(fit)[1:5, 1:5]
  test_overall <- wald.test(b = EST, Sigma = VAR, L = matrix(c(1,  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1), 
                                                             nrow = 4, ncol = 5, byrow = T))
  test_A12 <- wald.test(b = EST, Sigma = VAR, L = matrix(c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1), nrow = 3, 
                                                         ncol = 5, byrow = T))
  test_B12 <- wald.test(b = EST, Sigma = VAR, L = matrix(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1), nrow = 2, ncol = 5, byrow = T))
  test_1112 <- wald.test(b = EST, Sigma = VAR, L = matrix(c(0, 0, 0, 1, 0), nrow = 1, ncol = 5, byrow = T))
  test_1121 <- wald.test(b = EST, Sigma = VAR, L = matrix(c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0), nrow = 2, ncol = 5, byrow = T))
  test_1122 <- wald.test(b = EST, Sigma = VAR, L = matrix(c(1, 0, 0, 0, 0, 0, 0, 1, 1, 1), nrow = 2, ncol = 5, byrow = T))
  test_1221 <- wald.test(b = EST, Sigma = VAR, L = matrix(c(1, 0, 0, 0, 0, 0, 0, 1, -1, 0), nrow = 2, ncol = 5, byrow = T))
  test_1222 <- wald.test(b = EST, Sigma = VAR, L = matrix(c(1, 0, 0, 0, 0, 0, 0, 1, 0, 1), nrow = 2, ncol = 5, byrow = T))
  test_2122 <- wald.test(b = EST, Sigma = VAR, L = matrix(c(0, 0, 0, 1, 1), nrow = 1, ncol = 5, byrow = T))
  results <- t(data.frame(test_overall$result, test_A12$result, 
                          test_B12$result, test_1112$result, test_1121$result, 
                          test_1122$result, test_1221$result, test_1222$result, 
                          test_2122$result))
  rownames(results) <- NULL
  TEST <- data.frame(c("A1B1=A1B2=A2B1=A2B2", "A1=A2", "B1=B2", 
                       "A1B1=A1B2", "A1B1=A2B1", "A1B1=A2B2", "A1B2=A2B1", 
                       "A1B2=A2B2", "A2B1=A2B2"), results)
  names(TEST) <- c("H0", "test statistic", "df", "p")
  return(TEST)
}
