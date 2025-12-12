#' @title Shows size of objects in the R environment
#' @description Shows the size of the objects currently in the R environment. Helps to locate large objects cluttering the R environment and/or causing memory problems during the execution of large workflows.
#' @param n Number of objects to show, Default: `10`
#' @return A data frame with the row names indicating the object name, the field 'Type' indicating the object type, 'Size' indicating the object size, and the columns 'Length/Rows' and 'Columns' indicating the object dimensions if applicable.
#' @examples
#'
#'  #creating dummy objects
#'  x <- matrix(runif(100), 10, 10)
#'  y <- matrix(runif(10000), 100, 100)
#'
#'  #reading their in-memory size
#'  objects_size()
#'
#' @rdname objects_size
#' @importFrom utils object.size
#' @export
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

    obj.size <- napply(
      names,
      object.size
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
