#' @title Display sizes of objects in current R environment
#' @description Returns a summary of objects in the current R workspace, sorted from largest to smallest by memory size. Useful for identifying memory-intensive objects and diagnosing memory issues.
#' @param n Integer specifying the number of largest objects to display. Default: `10`.
#' @return Data frame with object names as row names and four columns:
#' \itemize{
#'   \item `Type`: Object class (e.g., "data.frame", "matrix", "list").
#'   \item `Size`: Memory size with automatic unit formatting (e.g., "1.2 Mb", "500 bytes").
#'   \item `Length/Rows`: Number of elements (for vectors) or rows (for data frames/matrices).
#'   \item `Columns`: Number of columns (for data frames/matrices; `NA` for vectors and other objects).
#' }
#' @details
#' This utility function helps monitor memory usage by displaying the largest objects in your workspace. It's particularly useful for:
#' \itemize{
#'   \item Identifying memory bottlenecks during large spatial analyses
#'   \item Deciding which objects to remove to free memory
#'   \item Understanding the memory footprint of different data structures
#' }
#'
#' The function examines all objects in the global environment (`.GlobalEnv`) and calculates their memory usage using [utils::object.size()]. Objects are automatically sorted by size in descending order.
#' @seealso [utils::object.size()], [base::ls()], [base::rm()]
#' @examples
#' # Create some objects of different sizes
#' small_vector <- runif(100)
#' medium_matrix <- matrix(runif(10000), 100, 100)
#' large_matrix <- matrix(runif(100000), 1000, 100)
#'
#' # View the 5 largest objects
#' objects_size(n = 5)
#'
#' # Check all objects (up to 10 by default)
#' objects_size()
#'
#' @rdname objects_size
#' @family utilities
#' @export
#' @autoglobal
objects_size <- function(n = 10) {
  .ls.objects <- function(
    pos = 1,
    pattern,
    order.by,
    decreasing = FALSE,
    head = FALSE,
    n = 5
  ) {
    napply <- function(names, fn) {
      sapply(
        names,
        function(x) fn(get(x, pos = pos))
      )
    }

    names <- ls(
      pos = pos,
      pattern = pattern
    )

    obj.class <- napply(
      names,
      function(x) as.character(class(x))[1]
    )

    obj.mode <- napply(
      names,
      mode
    )

    obj.type <- ifelse(
      is.na(obj.class),
      obj.mode,
      obj.class
    )

    obj.prettysize <- napply(
      names,
      function(x) {
        format(utils::object.size(x), units = "auto")
      }
    )

    obj.dim <- t(
      napply(
        names,
        function(x) as.numeric(dim(x))[1:2]
      )
    )

    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")

    obj.dim[vec, 1] <- napply(names, length)[vec]

    out <- data.frame(
      obj.type,
      obj.prettysize,
      obj.dim
    )
    names(out) <- c("Type", "Size", "Length/Rows", "Columns")
    if (!missing(order.by)) {
      out <- out[order(out[[order.by]], decreasing = decreasing), ]
    }
    if (head) {
      out <- head(out, n)
    }
    out
  }

  .ls.objects(
    order.by = "Size",
    decreasing = TRUE,
    head = TRUE,
    n = n
  )
}
