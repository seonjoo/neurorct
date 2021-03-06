
## code to prepare `template_nifti` dataset goes here
library(fslr)
library(oro.nifti)
sub1 = fslr::fsl_sub2('data-raw/funcInMNI.nii.gz', intern = TRUE)
sub2 = fslr::fsl_sub2(sub1, intern = TRUE)
template_nifti <- oro.nifti::readNIfTI(sub2)

usethis::use_data(template_nifti, internal = TRUE, overwrite = TRUE)


# code to generate files for shiny app using neurorct package
# X matrix
xmat = generate_xmat(npergroup = 30)
write.table(xmat, "neurorct_data/example_xmat30.csv")
# image
y = generate_y(npergroup = 20, saveinfti = TRUE, outfile = "neurorct_data/example_y30")
# mask
mask = generate_mask(neurorct:::template_nifti, saveinfti = TRUE, file = "neurorct_data/example_mask")

# code to prepare group_assignment.csv
library(tidyverse)
raw_data = read.sas7bdat('voxmerged_weightmerged_edufixed.sas7bdat')
tidy_data =
  raw_data %>%
  filter(is.na(random_group) == FALSE) %>%
  dplyr::select(c("subject_id",  "time_point", "random_group")) %>%
  filter(time_point == 1) %>%
  select(-time_point)
write_csv(tidy_data, 'group_assignment.csv')
