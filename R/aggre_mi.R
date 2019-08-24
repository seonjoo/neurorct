
#' Aggregate linear regression results after multiple imputation
#'
#' @param betamap V x m matrix of the coefficient estimates (intercept, txt, covariates etc.) over m imputation
#' @param stderrmat V x m matrix of the standard error (intercept, txt, covariates etc.) over m imputation
#' @param n sample size
#' @param k number of predictors (group + covariates)
#'
#' @return
#' @export
#'
#' @examples
#' stderrmat=matrix(rnorm(30)^2,10,3)
#' betamap=matrix(rnorm(30),10,3)
#' aggre_mi(betamap = betamap, stderrmat = stderrmat)
#'

aggre_mi <- function(betamap,stderrmat,n=60,k=3){

  m = ncol(betamap)
  var.within = apply(stderrmat^2, 1, mean)
  var.bet = apply(betamap,1,var)
  var.total = var.within + var.bet + var.bet/m
  #  riv=(var.bet + var.bet/m)/var.within
  lambda=(var.bet + var.bet/m)/var.total
  df.old=(m-1)/(lambda^2)
  df.obs=(n-k)/(n-k+2)*(n-k-1)*(1-lambda)
  df=(df.old*df.obs)/(df.old+df.obs)
  betahat=apply(betamap,1,mean)
  data.frame(Estimate=betahat,
             std.error=sqrt(var.total),
             statistic=betahat/sqrt(var.total),
             df=df,
             p.value=2*(1-pt(abs(betahat/sqrt(var.total)),df)))
}



