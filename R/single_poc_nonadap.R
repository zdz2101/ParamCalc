#' Single Arm Calculator (Non-Adaptive)
#'
#' @param soc Standard of care efficacy (0 - 1 value)
#' @param exp Expected experimental treatment efficacy (0 - 1 value)
#' @param alpha Desired alpha level (0 - 1 value)
#' @param pwr Desired power (0 - 1 value)
#' @return Displays the necessary amount of patients needed for the sample size to satisfy user defined parameters.
#' @export
#' @examples
#' library(ParamCalc)
#' single_poc()
#' single_poc(soc = 0.27, exp = 0.36, alpha = 0.03, pwr = 0.85)

single_poc <- function(soc = 0.25, exp = 0.40 , alpha = 0.05, pwr = 0.80){
  cond1 <- FALSE
  n <- 10
  z_temp <- 0
  while(cond1 == FALSE){
    z <- seq(1:n)
    for(i in 1:n){
      check1<- 1 - pbinom(z[i],n,soc)
      check2<- 1 - pbinom(z[i],n,exp)
      if(check1 < alpha & check2 > pwr){
        z_temp <- z[i]
        cond1 <- TRUE
        break
      }
    }
    n <- n + 1
  }
  print(paste(paste(paste(paste("Sample size:", n), "; with expected patient responses from"), z_temp)))
}
