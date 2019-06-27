#' Fast lm
#'
#' This function computes linear regression and computes t-scores for multivariate outcomes.
#' @title Fast linear regression
#'
#' @param x nxp input design matrix. It should not include intercept.
#' @param y qxn outcome matrix.
#' @param bl qxn baseline matrix.
#' @param ncore number of clusters registered for parallel processing.
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
fast_lm <- function(x, y, bl = NULL, ncore = 2) {

  if (dim(x)[1] != dim(y)[2]) error("dimension doesn't match!")
  if (!is.null(bl)) {
    if (ncol(bl) != ncol(y) | nrow(bl) != nrow(y)) error("dimension doesn't match! dim(bl) should equal dim(y).")
  }

  if (is.null(bl)) {
    xmat = cbind(1,x)
    n = nrow(x)
    p = ncol(x)
    invxxt = solve(t(xmat) %*% xmat)
    betahat = y %*% xmat %*% invxxt
    resid = y - betahat %*% t(xmat)
    sigmahat = as.vector(apply(resid,1,function(x)sqrt(sum(x*x)/(n - p - 1))))
    tstats = (1/sigmahat) * betahat %*% diag(1/sqrt(diag(invxxt)))
    df = n - p - 1
    return(list(tmap = tstats, df = df))
  } else
  {
    # need to verify this code chunk
    n = nrow(x)
    p = ncol(x) + 1

    cl <- makeCluster(ncore)
    doParallel::registerDoParallel(cl)
    tmap = foreach::foreach(i = 1:nrow(bl),
                  .combine = "rbind") %dopar% {
                    xmat = cbind(1, x, bl[i,])
                    invxxt = try(solve(t(xmat) %*% xmat), silent = TRUE)
                    if (isTRUE(class(invxxt) != "try-error")) {
                      betahat = y[i,] %*% xmat %*% invxxt
                      resid = y[i,] - betahat %*% t(xmat)
                      sigmahat = sqrt( sum(resid*resid)/(n-p-1) )
                      tstats = (1/sigmahat) * betahat %*% diag(1/sqrt(diag(invxxt)))
                      tstats
                    } else { rep(0, p+1) }
                  }
    stopCluster(cl)
    rownames(tmap) = c()
    df = n - p - 1 # I'm not sure about df here
    return(list(tmap = tmap, df = df))
  }
}


