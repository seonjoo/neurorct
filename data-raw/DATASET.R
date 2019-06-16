
## code to prepare `template_nifti` dataset goes here
library(fslr)
library(oro.nifti)
sub1 = fslr::fsl_sub2('data-raw/funcInMNI.nii.gz', intern = TRUE)
sub2 = fslr::fsl_sub2(sub1, intern = TRUE)
template_nifti <- oro.nifti::readNIfTI(sub2)
usethis::use_data(template_nifti, internal = TRUE, overwrite = TRUE)
