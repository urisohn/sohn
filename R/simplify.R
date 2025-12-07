#' Simplify Statistical Test Output
#'
#' A generic function that simplifies the output of various statistical tests
#' to make them more readable and informative.
#'
#' @param object The output object from a statistical test (e.g., from \code{t.test}).
#' @param digits Number of decimal places to display. Default is 3.
#' @param ... Additional arguments passed to specific simplify methods.
#'
#' @return A simplified version of the test output with enhanced formatting.
#'
#' @details
#' This function provides a unified interface for simplifying statistical test outputs.
#' It automatically detects the type of test object and routes it to the appropriate
#' simplification method.
#'
#' @examples
#' # Simplify t-test output
#' result <- t.test(rnorm(100), rnorm(100))
#' simplify(result)
#' 
#' # Simplify table output with variable names
#' df <- data.frame(
#'   smg1 = c(1, 2, 2, 1, 2),
#'   coupled = c(0, 1, 0, 1, 1)
#' )
#' simplify(table(df$smg1 == 2, df$coupled))
#'
#' @export
simplify <- function(object, digits = 3, ...) {
  UseMethod("simplify")
}

#' Default method for simplify
#'
#' @param object The object to simplify
#' @param digits Number of decimal places
#' @param ... Additional arguments
#'
#' @export
simplify.default <- function(object, digits = 3, ...) {
  stop("No simplify method available for objects of class: ", 
       paste(class(object), collapse = ", "))
}

#' Simplify t-test output
#'
#' Takes the output from \code{\link[stats]{t.test}} and produces a simplified,
#' more readable output with variable names and formatted results.
#'
#' @param object An object of class \code{"htest"} from \code{\link[stats]{t.test}}.
#' @param digits Number of decimal places to display for means and difference
#'   of means. Default is 3.
#' @param ... Additional arguments (not currently used).
#'
#' @return The same object with enhanced printing (invisibly).
#'
#' @details
#' This function enhances t-test output by:
#' \itemize{
#'   \item Displaying actual variable names instead of "mean of x" and "mean of y"
#'   \item Adding the observed difference of means to the output
#'   \item Formatting results in a cleaner, more readable format
#' }
#'
#' The function extracts variable names from the \code{data.name} attribute of
#' the t-test object when possible.
#'
#' @examples
#' # Two-sample t-test
#' men <- rnorm(100, mean = 5, sd = 1)
#' women <- rnorm(100, mean = 4.8, sd = 1)
#' result <- t.test(men, women)
#' simplify(result)
#'
#' # Formula syntax
#' data <- data.frame(y = rnorm(100), group = rep(c("A", "B"), 50))
#' result <- t.test(y ~ group, data = data)
#' simplify(result)
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @export
simplify.htest <- function(object, digits = 3, ...) {
  # Check if it's a t-test
  if (!grepl("t-test", object$method, ignore.case = TRUE)) {
    stop("simplify() for htest objects currently only supports t-test results")
  }
  
  # Store the calling environment for simplify_ttest to access original variables
  # This allows us to extract group values for formula syntax
  calling_env <- parent.frame()
  
  # Route to simplify_ttest
  simplify_ttest(object, digits = digits, calling_env = calling_env, ...)
}

#' Simplify table output
#'
#' Takes the output from \code{\link[base]{table}} and displays it with
#' variable names in the margins.
#'
#' @param object An object of class \code{"table"} from \code{\link[base]{table}}.
#' @param digits Number of decimal places to display. Default is 3.
#' @param ... Additional arguments (not currently used).
#'
#' @return The same table object with enhanced printing (invisibly).
#'
#' @details
#' This function extracts variable names from the call that created the table
#' and displays them in the margins of the table output.
#'
#' @examples
#' # Create sample data
#' df <- data.frame(
#'   smg1 = c(1, 2, 2, 1, 2),
#'   coupled = c(0, 1, 0, 1, 1)
#' )
#' 
#' # Simplify table with variable names
#' simplify(table(df$smg1 == 2, df$coupled))
#'
#' @seealso \code{\link[base]{table}}
#'
#' @export
simplify.table <- function(object, digits = 3, ...) {
  if (!inherits(object, "table")) {
    stop("object must be of class 'table'")
  }
  
  # Get the call from the parent frame
  calling_env <- parent.frame()
  
  # Try to get the call that created this table
  var_names <- NULL
  
  tryCatch({
    # Get the call from the parent frame (the call to simplify())
    parent_call <- sys.call(-1)
    
    # If the parent call is simplify(), get the argument which should be a table() call
    if (!is.null(parent_call) && as.character(parent_call[[1]]) == "simplify") {
      # Get the argument to simplify() which should be a table() call
      table_call <- parent_call[[2]]
      
      if (is.call(table_call) && as.character(table_call[[1]]) == "table") {
        # Extract arguments from table() call
        table_args <- as.list(table_call)[-1]  # Remove the function name
        
        # Extract variable names by deparsing
        var_names <- sapply(table_args, function(arg) {
          # Deparse the argument to get the variable name
          deparsed <- deparse(arg, width.cutoff = 500)
          # Clean up: remove leading/trailing spaces
          cleaned <- gsub("^\\s+|\\s+$", "", deparsed)
          # If it contains ==, extract the left side (the variable name)
          if (grepl("==", cleaned)) {
            cleaned <- sub("\\s*==.*$", "", cleaned)
          }
          # Return cleaned variable name
          cleaned
        })
      }
    }
  }, error = function(e) {
    # If we can't extract names, var_names stays NULL
    var_names <<- NULL
  })
  
  # Store variable names and digits as attributes
  attr(object, "var_names") <- var_names
  attr(object, "digits") <- digits
  
  # Change class to use our print method
  class(object) <- c("simplified_table", class(object))
  
  # Print the enhanced output
  print(object)
  
  invisible(object)
}
