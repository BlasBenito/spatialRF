#create package
usethis::create_package("/home/blas/Dropbox/GITHUB/R_packages/spatialRF")

#ignore dev_history at build
usethis::use_build_ignore("dev_history.R")

#define license
usethis::use_gpl3_license(name = "Blas M. Benito")

#first check
devtools::check()
