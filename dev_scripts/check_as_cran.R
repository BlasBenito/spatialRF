library(dplyr)

root <- "/home/blas/Dropbox/blas/GITHUB/R_packages/spatialRF"
setwd(root)

version <- "1.1.5"

gz_path <- paste0(
  root,
  "_",
  version,
  ".tar.gz"
)

x <- devtools::build(
  pkg = ".",
  path = gz_path
)

system(
  command = paste0(
    "_R_CHECK_SYSTEM_CLOCK_=0; R CMD check --as-cran --timings ",
    x
  )
)

read.table(
  paste0(root, "/spatialRF.Rcheck/spatialRF-Ex.timings"),
  header = TRUE
) |>
  dplyr::arrange(dplyr::desc(elapsed))
