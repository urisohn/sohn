#' Format P-Values for Display (p<.0001)
#'
#' Formats p-values for clean display in figures and tables.
#'
#' @param p A numeric vector of p-values to format.
#' @param digits Number of decimal places to round to. Default is 4.
#' @param include_p Logical. If TRUE, includes "p" prefix before the formatted
#'   value (e.g., "p = .05"). Default is FALSE.
#'
#' @return A character vector of formatted p-values.
#'
#' @details
#' This function formats p-values by:
#' \itemize{
#'   \item Rounding to the specified number of decimal places
#'   \item Removing the leading zero (e.g., "0.05" becomes ".05")
#'   \item Handling edge cases: p < 0.0001 becomes "< .0001", p > 0.9999 becomes "> .9999"
#'   \item Handling NA values gracefully
#' }
#'
#' @examples
#' # Basic usage
#' format.pvalue(0.05)
#' format.pvalue(0.0001)
#' format.pvalue(0.9999)
#'
#' # Vector input
#' format.pvalue(c(0.05, 0.001, 0.00001, 0.99))
#'
#' # With p prefix
#' format.pvalue(0.05, include_p = TRUE)
#'
#' # Different precision
#' format.pvalue(0.05, digits = 3)
#'
#' # Edge cases
#' format.pvalue(c(0, 0.00005, 0.5, 0.99999, 1, NA))
#'
#' @export
format.pvalue <- function(p, digits = 4, include_p = FALSE) {
  # Handle NA values
  is_na <- is.na(p)
  result <- character(length(p))
  
  # Create p prefix if requested
  p_prefix <- if (include_p) "p " else ""
  
  # Handle edge cases first
  result[p < 10^(-digits)] <- paste0(p_prefix, "< .0001")
  result[p > (1 - 10^(-digits))] <- paste0(p_prefix, "> .9999")
  
  # Handle regular p-values
  regular <- !is_na & p >= 10^(-digits) & p <= (1 - 10^(-digits))
  
  if (any(regular)) {
    p.clean <- round(p[regular], digits)
    p.clean <- format(p.clean, nsmall = digits, scientific = FALSE)
    # Remove leading zero
    p.clean <- sub("^0\\.", ".", p.clean)
    # Always add equals sign
    p.clean <- paste0(p_prefix, "= ", p.clean)
    result[regular] <- p.clean
  }
  
  # Handle NA values
  result[is_na] <- NA_character_
  
  return(result)
}

