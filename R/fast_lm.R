#' Fast lm
#'
#' This function computes linear regression and compute t-scores for multivariate outcomes.
#' @title Fast linear regression
#'
#' @param x nxp input design matrix. It should not include intercept.
#' @param y qxn outcome matrix.
#'
#' @return tmaps t-values for each coefficients
#' @return df Degree of freedome of each coefficients
#' @export
#'
#' @examples
#' library(neurorct)
#' y=matrix(rnorm(100*1000),1000,100)
#' x=cbind(rnorm(100),rnorm(100))
#' system.time(a<-t(apply(y, 1, function(zz)summary(lm(zz~x))$coefficients[,3])))
#' system.time(a2<-fast_lm(x=x,y=y))
#' sum(abs(a-a2$tmap))
#'
fast_lm<-function(x,y){
    xmat=cbind(1,x)
    n=nrow(x)
    p=ncol(x)
    invxxt=solve(t(xmat)%*% xmat)
    betahat=y %*% xmat %*% invxxt
    resid= y - betahat %*% t(xmat)
    sigmahat=as.vector(apply(resid,1,function(x)sqrt(sum(x*x)/(n-p-1))))
    tstats=(1/sigmahat) * betahat%*%diag(1/sqrt(diag(invxxt)))
    df=n-p-1
    return(list(tmap=tstats, df=df))
}


