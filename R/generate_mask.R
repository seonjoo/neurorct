
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
  mask <- img
  mask@.Data <- 0*img@.Data
  mask@.Data[img@.Data > 1000] <- 1
  return(mask)
}
