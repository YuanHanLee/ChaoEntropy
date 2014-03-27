entropy_Grassberger <-
function(data, B = 200, conf = 0.95) {
  est <- GrassEstFun(data)
  se <- BootstrapFun(data, B, GrassEstFun)
  z <- qnorm(1 - (1 - conf)/2)
  CI <- c(max(est - z * se, 0), est + z * se)
  out <- matrix(c(est, se, CI), nrow = 1)
  rownames(out) <- c("Grassberger (2003)")
  colnames(out) <- c("Estimator", "Bootstrap s.e.",
                     paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
  return(out)
}
