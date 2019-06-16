#' Generate simulated nifti data examples
#'
#' @param n sample size in each of the two groups. There are 2*n rows in the simulated data.
#'
#' @return an array of dim c(64, 64, 32, 2*n)
#' @export
#'
#' @examples
generate_y <- function(n = 10){
  # load .rda that stores an nifti object called `template_nifti`
  load("R/sysdata.rda")
  # generate a mask from the template
  mask = generate_mask(template_nifti)
  # choose a random area that has more perturb
  arraydim = dim(template_nifti)
  lower_bound = arraydim / 6
  width = round(arraydim / 16)
  ind1 = round(lower_bound + runif(3) * arraydim * (2/3))
  ind2 = ind1 + width
  # generate data
  perturb = sqrt(var(template_nifti[mask == 1])) * 0.1
  y = array(0, c(dim(template_nifti), n*2))
  for (j in 1:(2*n)) {
    set.seed(j)
    tmp <- template_nifti@.Data + perturb * array(rnorm(arraydim[1] * arraydim[2] * arraydim[3]),
                                                  dim = c(arraydim[1], arraydim[2], arraydim[3]))
    if (j < n + 1) {
      tmp[ind1[1]:ind2[1],ind1[2]:ind2[2],ind1[3]:ind2[3]] <-
      tmp[ind1[1]:ind2[1],ind1[2]:ind2[2],ind1[3]:ind2[3]] + perturb
      }
    y[,,,j] <- tmp
  }

  return(y)
}

