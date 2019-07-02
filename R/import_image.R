
#' Import imaging data for analysis
#'
#' @param folderpath folder path that contains imaging files.
#' @param maskpath mask file.
#' @param complete if TRUE, only read in files for complete-case data analysis.
#' @param sub2 if TRUE, subsample the image by factor of 2.
#'
#' @importFrom oro.nifti readNIfTI
#' @importFrom fslr fslsub2
#'
#' @return a list of baseline matrix and follow-up matrix.
#' @export
#'
#' @examples
import_image = function(folderpath, maskpath = NULL, complete = TRUE, sub2 = TRUE){
  filelist = list_files(folderpath, complete = complete)
  bl_filelist = filelist[[1]]
  fu_filelist = filelist[[2]]

  if (sub2) {
    suppressMessages({
      study58_bl_CC = lapply( paste(folderpath, bl_filelist, sep = ''), function(x) fslr::fslsub2(x))
      study58_fu_CC = lapply( paste(folderpath, fu_filelist, sep = ''), function(x) fslr::fslsub2(x))
      if (!(is.null(maskpath))) template <- fslsub2(maskpath, intern = TRUE)
      dims = dim(study58_bl_CC[[1]])
    })
  } else {
    study58_bl_CC = lapply( paste(folderpath, bl_filelist, sep = ''), function(x) readNIfTI(x) )
    study58_fu_CC = lapply( paste(folderpath, fu_filelist, sep = ''), function(x) readNIfTI(x) )
    if (!(is.null(maskpath))) template <- readNIfTI(maskpath)
    dims = dim(study58_bl_CC[[1]])
    }

  if (!(is.null(maskpath))) {
    mask <- template@.Data[1:40+3,1:48+3,1:34+6]
    mask[mask < 0.1] = 0; mask[mask > 0.1] = 1
    datmat_bl = do.call(cbind, lapply(study58_bl_CC, function(x) as.vector(x[mask == 1])))
    datmat_fu = do.call(cbind, lapply(study58_fu_CC, function(x) as.vector(x[mask == 1])))
    datmat_bl[which(is.nan(datmat_bl))] = 0
    datmat_fu[which(is.nan(datmat_fu))] = 0
    } else {
      datmat_bl = do.call(cbind, lapply(study58_bl_CC, function(x) as.vector(x)))
      datmat_fu = do.call(cbind, lapply(study58_fu_CC, function(x) as.vector(x)))
      datmat_bl[which(is.nan(datmat_bl))] = 0
      datmat_fu[which(is.nan(datmat_fu))] = 0
    }

  return(list(bl = datmat_bl, fu = datmat_fu, dims = dims))

}
