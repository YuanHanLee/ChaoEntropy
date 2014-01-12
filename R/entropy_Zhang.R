entropy_Zhang <-
function(data, B = 200, conf = 0.95) {
  est <- ZhangHzstarEstFun(data)
  se <- BootstrapFun(data, B, ZhangHzstarEstFun)
  z <- qnorm(1 - (1 - conf)/2)
  CI <- c(max(est - z * se, 0), est + z * se)
  out <- matrix(c(est, se, CI), nrow = 1)
  rownames(out) <- c("Zhang (2012) Hz*")
  colnames(out) <- c("Estimator", "Bootstrap s.e.",
                     paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
  return(out)
}
