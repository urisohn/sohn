#' Create a list where objects are automatically named based on their existing names 
#' 
#' @param ... Objects to include in the list. Can be named or unnamed.
#'
#' @return A named list containing the input objects, with names derived from
#'   the input object names when not explicitly provided.
#'
#' @details
#' This function is inspired by a solution from Stack Overflow:
#' \url{https://stackoverflow.com/questions/16951080/can-lists-be-created-that-name-themselves-based-on-input-object-names}
#'
#' If objects are passed with explicit names (e.g., \code{namedList(a = x, b = y)}),
#' those names are used. If objects are passed without names, the function
#' attempts to use the variable names of the objects themselves.
#'
#' @examples
#' x <- 1:5
#' y <- letters[1:3]
#' z <- matrix(1:4, nrow = 2)
#'
#' # Create named list from objects
#' my_list <- namedList(x, y, z)
#' names(my_list)  # "x" "y" "z"
#'
#' # Works with explicit names too
#' my_list2 <- namedList(a = x, b = y)
#' names(my_list2)  # "a" "b"
#'
#' @export
namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  
  if (is.null(nm <- names(L))) nm <- snm
  
  if (any(nonames <- nm == "")) nm[nonames] <- snm[nonames]
  
  setNames(L, nm)
}

