
#' Helper function: Generate mask nifti object
#'
#' @param img object of class nifti to generate mask
#' @param saveinfti if saveinfti = TRUE, save result to NIfTI file.
#' @param file the name or connection of the file where the csv file is saved to.
#'
#' @return object of class nifti as mask
#' @export
#'
#' @examples
#' library(neurorct)
#'
#'
generate_mask = function(img = neurorct:::template_nifti, saveinfti = FALSE, file = ''){
  mask <- img
  mask@.Data <- 0*img@.Data
  mask@.Data[img@.Data > 1000] <- 1

  if (saveinfti == TRUE) {
    writeNIfTI(mask, file = file)
  }

  return(mask)
}
