#' Generate simulated nifti data examples
#'
#' @param npergroup sample size in each of the two groups. There are 2*n rows in the simulated data.
#' @param saveinfti if saveinfti = TRUE, save result to NIfTI file.
#' @param outfile the name of the file where the NIfTI file is saved to.
#' @param type specify study type. 'xs': Cross-sectional; 'long':longitudinal.
#'
#' @return an array of dim c(64, 64, 32, 2*n)
#' @export
#'
#' @examples
#'
#'
generate_y <- function(npergroup = 10, type = 'xs', saveinfti = FALSE, outfile = ""){

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
  # standard deviation of area where mask = 1
  perturb = sqrt(var(template_nifti[mask == 1])) * 0.1

  # generate cross-sectional data
  if (type == 'xs'){
    y = array(0, c(dim(template_nifti), n*2))
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
      y_nifti <- copyNIfTIHeader(template_nifti, y_nifti)
      datatype(y_nifti) <- 16
      y_nifti@bitpix <- 32
      writeNIfTI(y_nifti, file = outfile)
    }

    if (type == 'long'){
      y_nifti <- nifti(abind(y.base, y.fu, along = 4))
      y_nifti <- copyNIfTIHeader(template_nifti, y_nifti)
      datatype(y_nifti) <- 16
      y_nifti@bitpix <- 32
      writeNIfTI(y_nifti, file = outfile)
    }
  }

  return(y)
}

