entropy_ChaoShen <-
function(data, B = 200, conf = 0.95) {
  if (sum(data > 0) == 1) {
    est <- 0
    se <- 0
  } else {
    est <- ChaoShenEstFun(data)
    se <- BootstrapFun(data, B, ChaoShenEstFun)
  }
  z <- qnorm(1 - (1 - conf)/2)
  CI <- c(max(est - z * se, 0), est + z * se)
  out <- matrix(c(est, se, CI), nrow = 1)
  rownames(out) <- c("Chao_Shen (2003)")
  colnames(out) <- c("Estimator", "Bootstrap s.e.",
                     paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
  return(out)
}
