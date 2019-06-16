#' Generate simulated nifti data examples
#'
#' @param npergroup sample size in each of the two groups. There are 2*n rows in the simulated data.
#' @param saveinfti if saveinfti = TRUE, save result to NIfTI file.
#' @param outfile the name or connection of the file where the NIfTI file is saved to.
#' @param type specify study type. 'xs': Cross-sectional; 'long':longitudinal.
#'
#' @return if cross-sectional, an array of dim c(64, 64, 32, 2*npergroup); if longitudinal, a list of 2 such arrays for basline and follow-up.
#' @export
#'
#' @examples
#' library(neurorct)
#' # cross-sectional data
#' xs_data = generate_y(35, type = 'xs')
#'
#' # longitudinal data
#' long_data = generate_y(20, type = 'long')
#' bl = long_data$baseline
#' fu = long_data$follow_up
#'
#'
generate_y <- function(npergroup = 10, type = c('xs', 'long'), saveinfti = FALSE, outfile = ""){
  type <- match.arg(type)
  # generate a mask from the template nifti object
  mask = generate_mask(neurorct:::template_nifti)
  # choose a random area that has more perturb
  arraydim = dim(template_nifti)
  lower_bound = arraydim / 6
  width = round(arraydim / 16)
  ind1 = round(lower_bound + runif(3) * arraydim * (2/3))
  ind2 = ind1 + width
  # standard deviation of area where mask = 1
  perturb = sqrt(var(template_nifti[mask == 1])) * 0.1

  # generate cross-sectional data
  if (type == 'xs'){
    y = array(0, c(dim(template_nifti), npergroup*2))
    for (j in 1:(2*npergroup)) {
      set.seed(j)
      tmp <- template_nifti@.Data + perturb * array(rnorm(arraydim[1] * arraydim[2] * arraydim[3]),
                                                    dim = c(arraydim[1], arraydim[2], arraydim[3]))
      if (j < npergroup + 1) {
        tmp[ind1[1]:ind2[1],ind1[2]:ind2[2],ind1[3]:ind2[3]] <-
          tmp[ind1[1]:ind2[1],ind1[2]:ind2[2],ind1[3]:ind2[3]] + perturb
      }
      y[,,,j] <- tmp
    }
  }

  # OR generate longitudinal data
  if (type == 'long'){
    y.base = y.fu = array(0, c(dim(template_nifti), npergroup*2))
    for (j in 1:(npergroup*2)){
      set.seed(j)
      ## Baseline
      tmp <- template_nifti@.Data + perturb * array(rnorm(arraydim[1] * arraydim[2] * arraydim[3]),
                                                    dim = c(arraydim[1], arraydim[2], arraydim[3]))
      y.base[,,,j] <- tmp
      ## followup
      tmp2 <- tmp + 0.5 * perturb * array(rnorm(arraydim[1] * arraydim[2] * arraydim[3]),
                                          dim = c(arraydim[1], arraydim[2], arraydim[3]))
      if (j < npergroup + 1){
        tmp2[ind1[1]:ind2[1],ind1[2]:ind2[2],ind1[3]:ind2[3]] <-
          tmp2[ind1[1]:ind2[1],ind1[2]:ind2[2],ind1[3]:ind2[3]] + perturb
      }
      y.fu[,,,j] <- tmp2
    }
  }

  if (saveinfti == TRUE){
    if (type == 'xs'){
      y_nifti <- nifti(y)
      y_nifti <- neurobase::copyNIfTIHeader(template_nifti, y_nifti)
      datatype(y_nifti) <- 16
      y_nifti@bitpix <- 32
      writeNIfTI(y_nifti, file = outfile)
    }

    if (type == 'long'){
      y_nifti <- nifti(abind(y.base, y.fu, along = 4))
      y_nifti <- neurobase::copyNIfTIHeader(template_nifti, y_nifti)
      datatype(y_nifti) <- 16
      y_nifti@bitpix <- 32
      writeNIfTI(y_nifti, file = outfile)
    }
  }

  if (type == 'xs') return(y)
  if (type == 'long') return(list(baseline = y.base, follow_up = y.fu))
}

