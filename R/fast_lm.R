#' Fast lm
#'
#' This function computes linear regression and computes t-scores for multivariate outcomes.
#' @title Fast linear regression
#'
#' @param x nxp input design matrix. It should not include intercept.
#' @param y qxn outcome matrix. q is number of voxels.
#' @param bl (default=NULL) qxn baseline matrix.
#' @param ncore (default=2) number of clusters registered for parallel processing.
#' @param aggregate (default=FALSE) if TRUE, output betamap and stderrmat instead of tmap.
#' @param hdmi_output the output of hd_mi function
#' @param covidx (default=1) index of covariate of interest in the x matrix. Default is group if group is the first column in the x design matrix.
#'
#' @importFrom foreach %dopar%
#'
#' @return tmaps t-values for each coefficients
#' @return df Degree of freedome of each coefficients
#' @export
#'
#' @examples
#' library(neurorct)
#' y=matrix(rnorm(100*1000),1000,100)
#' x=cbind(cov1=rnorm(100),cov2=rnorm(100))
#' system.time(a<-t(apply(y, 1, function(zz)summary(lm(zz~x))$coefficients[,3])))
#' system.time(a2<-fast_lm(x=x,y=y))
#' sum(abs(a-a2$tmap))
#'
fast_lm <- function(x, y = NULL, bl = NULL, ncore = 1, aggregate = FALSE, hdmi_output = NULL, covidx = 1) {

  x = cbind(x)

  if (is.null(hdmi_output) == FALSE) {
    # compute y for each imputation
    y = lapply(hdmi_output, function(data){t(data[[2]] - data[[1]])})
    m = length(y)
    # iterate fast_lm
    results = lapply(y, function(i) {neurorct::fast_lm(x, y = i, bl, ncore, aggregate = TRUE)})
    # compute betamap and stderrmat for aggregation
    betamap = do.call(cbind, lapply(1:m, function(idx){results[[idx]]$betamap}))
    stderrmat = do.call(cbind, lapply(1:m, function(idx){results[[idx]]$stderrmat}))
    # use `aggre_mi` to return a data.frame
    return( aggre_mi(betamap = betamap, stderrmat = stderrmat, n = nrow(y[[1]]), k = ncol(x) + !(is.null(bl))) )
  }


  if (dim(x)[1] != dim(y)[2]) stop("dimension doesn't match!")
  if (!is.null(bl)) {
    if (ncol(bl) != ncol(y) | nrow(bl) != nrow(y)) stop("dimension doesn't match! dim(bl) should equal dim(y).")
  }

  if (is.null(bl)) {
    xmat = cbind(1,x)
    n = nrow(x)
    p = ncol(x)
    invxxt = solve(t(xmat) %*% xmat)
    betahat = y %*% xmat %*% invxxt
    resid = y - betahat %*% t(xmat)
    sigmahat = as.vector(apply(resid,1,function(e) sqrt(sum(e*e)/(n-p-1))))
    stderrmat = matrix(sigmahat) %*% matrix(sqrt(diag(invxxt)), ncol = ncol(invxxt)) ## Is it correct? # compare with lm results
    tstats = (1/sigmahat) * betahat %*% diag(1/sqrt(diag(invxxt)))
    df = n - p - 1
    pval = 2*(1 - pt(abs(tstats), df))

    betahat = betahat[,covidx+1]
    tstats = tstats[,covidx+1]
    pval = pval[,covidx+1]
  } else
  {
    n = nrow(x)
    p = ncol(x) + 1
    df = n - p - 1
    # cl <- parallel::makeCluster(ncore)
    # doParallel::registerDoParallel(cl)
    # stats = foreach::foreach(i = 1:nrow(bl), .combine = "rbind") %dopar% {
    #
                    # xmat = cbind(1, x, bl[i,])
                    # invxxt = try(solve(t(xmat) %*% xmat), silent = TRUE)
                    # betahat = y[i,] %*% xmat %*% invxxt
                    # resid = y[i,] - betahat %*% t(xmat)
                    # sigmahat = sqrt( sum(resid*resid)/(n-p-1) )
                    #   ## Is stderrmat correctly computed?
                    # stderrmat = matrix(sigmahat) %*% matrix(sqrt(diag(invxxt)), ncol = ncol(invxxt))
                    # tstats = (1/sigmahat) * betahat %*% diag(1/sqrt(diag(invxxt)))
                    # rbind(tstats,stderrmat,betahat)
    #
                    # tmp = broom::tidy(lm(y ~ ., data = data.frame(cbind(y = y[i,], x, bl[i,]))))[covidx+1,]
                    # tmp
    #               }
    stats = lapply(1:nrow(bl),
                   function(i){
                     # print(i)
                     # broom::tidy(lm(y ~ ., data = data.frame(cbind(y = y[i,], x, bl[i,]))))[covidx+1,]
                     fit <- try(lm(y ~ ., data = data.frame(cbind(y = y[i,], x, bl[i,]))),
                                silent = TRUE)
                     if (class(fit) == "try-error") {
                       cbind(NA, NA, NA, NA)
                     } else {
                       estimate = coef(fit)[covidx+1]
                       std.error = sqrt(vcov(fit)[covidx+1, covidx+1])
                       statistic = estimate / std.error
                       p.value = 2*(1-pt(abs(statistic), df))
                       cbind(estimate, std.error, statistic, p.value)
                     }
                   }
    ) %>% do.call(rbind, .) %>% as_tibble()

    tstats = stats$statistic
    betahat = stats$estimate
    stderrmat = stats$std.error
    pval = stats$p.value
  }

  if (aggregate == TRUE) {return(list(betamap = betahat, stderrmat = stderrmat))}
    else {return(list(maps = list(betamap = betahat, tmap = tstats, pmap = pval), df = df))}

}



