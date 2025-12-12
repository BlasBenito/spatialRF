doc <- function() {
  devtools::document()
  devtools::document()
}

minicheck <- function() {
  doc()
  devtools::check(args = c("--no-tests", "--no-examples"))
}

maxicheck <- function() {
  doc()
  devtools::check()
}

load <- function() {
  devtools::load_all()
}
