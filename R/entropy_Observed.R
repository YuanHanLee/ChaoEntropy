entropy_Observed <-
function(data, B = 200, conf = 0.95) {
  est <- ObservedEstFun(data)
  se <- BootstrapFun(data, B, ObservedEstFun)
  z <- qnorm(1 - (1 - conf)/2)
  CI <- c(max(est - z * se, 0), est + z * se)
  out <- matrix(c(est, se, CI), nrow = 1)
  rownames(out) <- c("Observed entropy")
  colnames(out) <- c("Estimator", "Bootstrap s.e.",
                     paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
  return(out)
}
