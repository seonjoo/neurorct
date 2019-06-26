#' Multiple imputation with lmer
#'
#' This function computes mixed effect regression after
#' voxel-wise multiple imputation.
#'
#' @title Multiple Imputation and running mixed effect regression
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
#' m.dim=c(20,30,40)
#' y.base=y.fu=array(0,c(dim(a),npergroup*2))
#' eff.d=0.8
#' npergroup=30
#' for (j in 1:(npergroup*2)){
#' set.seed(j)
#' y.base[,,,j]<-array(rnorm(m.dim[1]*m.dim[2]*m.dim[3]),dim=m.dim)
#' tmp2<-0.5*perturb*array(rnorm(m.dim[1]*m.dim[2]*m.dim[3]),dim=m.dim)
#' if (j<npergroup){tmp2[10:13,15:17,20:22]<-tmp2[10:13,15:17,20:22]+eff.d}
#' y.fu[,,,j]<-tmp2
#' }
#'
#'
mi_lmer <- function(x,y){


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


