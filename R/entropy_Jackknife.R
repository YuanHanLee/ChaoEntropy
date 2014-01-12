entropy_Jackknife <-
function(data, B = 200, conf = 0.95) {
  est <- ZahlJackEstFun(data)
  se <- BootstrapFun(data, B, ZahlJackEstFun)
  z <- qnorm(1 - (1 - conf)/2)
  CI <- c(max(est - z * se, 0), est + z * se)
  out <- matrix(c(est, se, CI), nrow = 1)
  rownames(out) <- c("Zahl (1977) Jackknife")
  colnames(out) <- c("Estimator", "Bootstrap s.e.",
                     paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
  return(out)
}
