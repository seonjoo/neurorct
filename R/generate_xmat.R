
#' Generate simulated X matrix (i.e. design matrix excluding intercept)
#'
#' @param npergroup sample size in each of the two groups.
#' @param type specify study type. 'xs': Cross-sectional; 'long':longitudinal.
#' @param savecsv if savecsv = TRUE, save result to csv file.
#' @param outfile the name or connection of the file where the csv file is saved to.
#'
#' @return a data.frame object of X matrix
#' @export
#'
#' @examples
#' library(neurorct)
#' generate_xmat(npergroup = 30, type = "xs")
#'
generate_xmat = function(npergroup, type = c('xs', "long"), savecsv = FALSE, file = ""){
  type <- match.arg(type)
  if (file == "") {
    stop("Please specify output file path", call. = FALSE)
  }

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

  if (savecsv == FALSE){
    try(write.csv(xmat, file))
  }

  return(xmat)
}

