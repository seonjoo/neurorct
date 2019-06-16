
#' Generate mask nifti object
#'
#' @param nifti_object nifti template
#'
#' @return a mask nifti object
#'
#'
#' @examples
#'
generate_mask = function(nifti_object){
  mask <- nifti_object
  mask@.Data <- 0*nifti_object@.Data
  mask@.Data[nifti_object@.Data > 1000] <- 1
  return(mask)
}
