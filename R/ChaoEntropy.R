ChaoEntropy <-
function(data, datatype = c("abundance", "incidence"), 
                        method = c("all", "Chao", "ChaoShen", "Grassberger", 
                                   "Jackknife", "Zhang", "Observed"), 
                        se = TRUE, nboot = 200, conf = 0.95) {
  if (is.matrix(data) == TRUE || is.data.frame(data) == TRUE) {
    data <- as.matrix(data)
    if (ncol(data) != 1 & nrow(data) != 1)
      stop("Error: The data format is wrong.")
    if (ncol(data) == 1) {
      data <- data[, 1]
    } else {
      data <- data[1, ]
    } 
  } 
  
  if (is.numeric(conf) == FALSE || conf > 1 || conf < 0) {
    cat("Warning: \"conf\"(confidence level) must be a numerical value between 0 and 1, e.g. 0.95.",
        "\n")
    cat("          We use \"conf\" = 0.95 to calculate!", 
        "\n\n")
    conf <- 0.95
  }
  
  if (se == TRUE) {
    B <- nboot
    if (nboot < 1)
      nboot <- 1
    if (nboot == 1)
      cat("Warning: When \"nboot\" =" ,B, ", the bootstrap s.e. and confidence interval can't be calculated.", 
          "\n\n")  
  }
  if (se == FALSE)
    nboot <- 1
  method <- match.arg(method)
  datatype <- match.arg(datatype)
  if (datatype == "abundance") {
    if (sum(data) == 0)
      stop("Error: The data didn't have enough information.")
    if (sum(data > 0) == 1) {
      cat("Warning: When the individual-based (abundance) data only have \"ONE\" species.", 
          "\n")
      cat("         ALL estimator are equal to 0,and the standard error will meaningless.", 
          "\n\n")
      out <- data.frame(Estimator=rep(0,6))
      rownames(out) <- c("Chao_entropy (2013)", "Chao_Shen (2003)", "Grassberger (2003)",
                         "Zahl (1977) Jackknife", "Zhang (2012) Hz*", "Observed entropy")
    } else {
      out <- ChaoEntropy.Ind(data, method, nboot, conf, se)
    }
  }
  if (datatype == "incidence") {
    if (sum(data[1] < data[-1]) != 0)
      stop("Error: Total number of sampling units should be greater than the species incidence frequency.")
    if (length(data) == 1) 
      stop("Error: The input format of first entry should be total number of sampling units, and followed by species incidence frequency. You only type the total number of sampling units.")
    if (sum(data[-1]) == 0)
      stop("Error: The data didn't have enough information.")
    if (method[1] == "ChaoShen" || method == "Grassberger" || 
          method == "Jackknife" || method == "Zhang") {
      cat("Warning: Sample-based incidence data doesn't have this estimator.",
          "\n")
      cat("         We use Chao and Observed estimator below.", "\n\n")
      method <- "all"
    }
    out <- ChaoEntropy.Sam(data, method, nboot, conf, se)
  }
  return(out)
}
