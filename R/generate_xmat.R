
#' Generate simulated X matrix (i.e. design matrix excluding intercept)
#'
#' @param npergroup sample size in each of the two groups
#' @param type specify study type. 'xs': Cross-sectional; 'long':longitudinal.
#'
#' @return a data.frame object of X matrix
#' @export
#'
#' @examples
generate_xmat = function(npergroup, type = 'xs'){

  if (type == "xs"){
    xmat = data.frame(group = rep(0:1, each = npergroup),
                      age = sample(20:50, 2*npergroup, replace = TRUE))
  }

  if (type == "long"){
    xmat = data.frame(subject = rep(1:(npergroup*2), 2),
                      group = rep(rep(0:1, each = npergroup), 2),
                      time = rep(0:1, each = npergroup*2),
                      age = rep(sample(20:50, npergroup*2, replace = TRUE), 2))
  }

}

