#create package
usethis::create_package("/home/blas/Dropbox/GITHUB/R_packages/spatialRF")

#ignore dev_history at build
usethis::use_build_ignore("dev_history.R")

#define license
usethis::use_gpl3_license(name = "Blas M. Benito")

#first check
devtools::check()

#connecting to github
usethis::use_git()

#add token to renfir
usethis::edit_r_environ()
#add line
#GITHUB_PAT=my_github_token

#restart R
.rs.restartR()

#create github repo
usethis::use_github(
  private = TRUE,
  protocol = "ssh"
)

#loading functions from their original location
source("/home/blas/Dropbox/GITHUB/gbif_plantae/functions.R")
source("/home/blas/Dropbox/GITHUB/gbif_plantae/rf_spatial.R")
source("/home/blas/Dropbox/GITHUB/gbif_plantae/rf_repeat.R")
source("/home/blas/Dropbox/GITHUB/gbif_plantae/rf_evaluate.R")



#writing functions into the R folder


devtools::load_all()


devtools::install()
