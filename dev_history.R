library(attachment)
library(usethis)
library(devtools)
library(roxygen2)
library(testthat)
library(sinew)
devtools::install_github("datastorm-open/DependenciesGraphs")
library(DependenciesGraphs)

#TO CHECK
########################################
# Document functions and dependencies
attachment::att_to_description()
devtools::document()
# Check the package
devtools::check()
########################################

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

#generating example data
ecoregions_richness$ecoregion_name.1 <- NULL
usethis::use_data(plant_richness) #created from ecoregions$data and ecoregions$centroids in the project gbif_plantae
usethis::use_data(distance_matrix)

#documenting the data
usethis::use_r("plant_richness")
usethis::use_r("distance_matrix")

#loading functions from their original location
usethis::use_build_ignore("functions.R")

#adding functions
usethis::use_r("moran")
sinew::makeOxygen(moran)

usethis::use_r("moran_multiscale")
sinew::makeOxygen(multiscale_moran)



#to check function dependencies
dep <- funDependencies(
  envir = environment(),
  name.function = "moran"
  )
plot(dep)

#writing functions into the R folder


# Document functions and dependencies
attachment::att_to_description()
devtools::document()
# Check the package
devtools::check()

devtools::load_all()


devtools::install()
