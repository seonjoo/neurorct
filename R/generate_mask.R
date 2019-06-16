
#' Helper function: Generate mask nifti object
#'
#' @param nifti_object object of class nifti to generate mask
#'
#' @return object of class nifti as mask
#'
#'
#' @examples
#'
generate_mask = function(img){
  mask <- nifti_object
  mask@.Data <- 0*nifti_object@.Data
  mask@.Data[nifti_object@.Data > 1000] <- 1
  return(mask)
}
