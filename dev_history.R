library(attachment)
library(usethis)
library(devtools)
library(roxygen2)
library(testthat)
library(sinew)
library(exampletestr)
# devtools::install_github("datastorm-open/DependenciesGraphs")
library(DependenciesGraphs)

#TO CHECK
########################################
attachment::att_from_description()
devtools::document()
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
# ecoregions_richness$ecoregion_name.1 <- NULL
# usethis::use_data(plant_richness) #created from ecoregions$data and ecoregions$centroids in the project gbif_plantae
# usethis::use_data(distance_matrix)
# usethis::use_build_ignore("data/old")
#
# plant_richness_df <- sf::st_drop_geometry(plant_richness)
# plant_richness_sf <- plant_richness
#
# usethis::use_data(plant_richness_df)
# usethis::use_data(plant_richness_sf)

#making the data smaller
# plant_richness_ <- plant_richness %>% dplyr::filter(x < -25)
# plant_richness_$fragmentation_shape_mn <- NULL
# plant_richness_$fragmentation_para_mn <- NULL
# plant_richness_$fragmentation_ed <- NULL
# plant_richness_$fragmentation_dcore_mn <- NULL
# plant_richness_$fragmentation_core_mn <- NULL
#
# plant_richness_$ecoregion_id <- 1:nrow(plant_richness_)
#
# distance_matrix_ <- distance_matrix[plant_richness_$ecoregion_id, plant_richness_$ecoregion_id]
# rownames(distance_matrix_) <- colnames(distance_matrix_) <- 1:nrow(plant_richness_)
#
# plant_richness <- plant_richness_
# distance_matrix <- distance_matrix_
# rm(plant_richness_, distance_matrix_)
# usethis::use_data(plant_richness)
# usethis::use_data(distance_matrix)

#documenting the data
usethis::use_r("plant_richness")
usethis::use_r("distance_matrix")

#loading functions from their original location
usethis::use_build_ignore("functions.R")

#adding functions

#moran
usethis::use_r("moran")
sinew::makeOxygen(moran)
exampletestr::make_test_shell_fun("moran", open = FALSE)

usethis::use_r("moran_multiscale")
sinew::makeOxygen(multiscale_moran)
exampletestr::make_test_shell_fun("moran_multiscale", open = FALSE)

usethis::use_r("root_mean_squared_error")
sinew::makeOxygen(root_mean_squared_error)
exampletestr::make_test_shell_fun("root_mean_squared_error", open = FALSE)

usethis::use_r("scale_robust")
sinew::makeOxygen(scale_robust)
exampletestr::make_test_shell_fun("scale_robust", open = FALSE)

usethis::use_r("auto_vif")
sinew::makeOxygen(auto_vif)
exampletestr::make_test_shell_fun("auto_vif", open = FALSE)

usethis::use_r("vif")
sinew::makeOxygen(vif)
exampletestr::make_test_shell_fun("vif", open = FALSE)

usethis::use_r("auto_cor")
sinew::makeOxygen(auto_cor)
exampletestr::make_test_shell_fun("auto_cor", open = FALSE)

usethis::use_r("statistical_mode")
sinew::makeOxygen(statistical_mode)
exampletestr::make_test_shell_fun("statistical_mode", open = FALSE)

usethis::use_r("thinning")
sinew::makeOxygen(thinning)
exampletestr::make_test_shell_fun("thinning", open = FALSE)

usethis::use_r("thinning_til_n")
sinew::makeOxygen(thinning_til_n)
exampletestr::make_test_shell_fun("thinning_til_n", open = FALSE)

usethis::use_r("cluster_specification")
sinew::makeOxygen(cluster_specification)
exampletestr::make_test_shell_fun("cluster_specification", open = FALSE)

usethis::use_r("rescale_vector")
sinew::makeOxygen(rescale_vector)
exampletestr::make_test_shell_fun("rescale_vector", open = FALSE)

usethis::use_r("pca")
sinew::makeOxygen(pca)
exampletestr::make_test_shell_fun("pca", open = FALSE)

usethis::use_r("pca_distance_matrix")
sinew::makeOxygen(pca_distance_matrix)
exampletestr::make_test_shell_fun("pca_distance_matrix", open = FALSE)

usethis::use_r("make_spatial_fold")
sinew::makeOxygen(make_spatial_fold)
exampletestr::make_test_shell_fun("make_spatial_fold", open = FALSE)

usethis::use_r("make_spatial_folds")
sinew::makeOxygen(make_spatial_folds)
exampletestr::make_test_shell_fun("make_spatial_folds", open = FALSE)

usethis::use_r("objects_size")
sinew::makeOxygen(objects_size)
exampletestr::make_test_shell_fun("objects_size", open = FALSE)

usethis::use_r("rf")
sinew::makeOxygen(rf)
exampletestr::make_test_shell_fun("rf", open = FALSE)

usethis::use_r("rf_repeat")
sinew::makeOxygen(rf_repeat)
exampletestr::make_test_shell_fun("rf_repeat", open = FALSE)

usethis::use_r("rank_spatial_predictors")
sinew::makeOxygen(rank_spatial_predictors)
exampletestr::make_test_shell_fun("rank_spatial_predictors", open = FALSE)

usethis::use_r("select_spatial_predictors_sequential")
sinew::makeOxygen(select_spatial_predictors_sequential)
exampletestr::make_test_shell_fun("select_spatial_predictors_sequential", open = FALSE)

usethis::use_r("select_spatial_predictors_optimized")
sinew::makeOxygen(select_spatial_predictors_optimized)
exampletestr::make_test_shell_fun("select_spatial_predictors_optimized", open = FALSE)



#to check function dependencies
dep <- DependenciesGraphs::funDependencies(
  envir = environment(),
  name.function = "rf_spatial"
  )
plot(dep)

#writing functions into the R folder




devtools::load_all()


devtools::install()
