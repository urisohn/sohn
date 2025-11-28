#' Miscellaneous Functions for Papers and Blogposts
#'
#' @description
#' The \pkg{sohn} package provides miscellaneous functions used in papers and
#' blogposts by Uri Simonsohn. It includes functions for data visualization,
#' statistical analysis, and data formatting.
#'
#' @details
#' This package contains various utility functions organized into the following categories:
#'
#' @section Graphing:
#' Functions for data visualization and plotting:
#' \itemize{
#'   \item \code{\link{scatter.gam}}: Scatter plots with GAM smooth lines
#'   \item \code{\link{cdf.by}}: Plot empirical cumulative distribution functions by group
#'   \item \code{\link{fhist}}: Frequency histograms without binning
#' }
#'
#' @section Statistical Analyses:
#' Functions for statistical modeling and result formatting:
#' \itemize{
#'   \item \code{\link{lmr}}: Linear models with robust standard errors
#'   \item \code{\link{format.pvalue}}: Format p-values for display
#' }
#'
#' @section Simulations:
#' Functions for running and monitoring simulations:
#' \itemize{
#'   \item \code{\link{counter}}: Adaptive progress reporter for simulations
#' }
#'
#' @section Data Management:
#' Functions for data manipulation and utilities:
#' \itemize{
#'   \item \code{\link{namedList}}: Create lists with automatic naming
#'   \item \code{\link{convert_to_sql}}: Convert CSV files to SQL INSERT statements
#'   \item \code{\link{message.col}}: Print colored messages to console
#' }
#'
#' @author
#' Uri Simonsohn \email{urisohn@gmail.com}
#'
#' @references
#' Data Colada blog: \url{https://datacolada.org/}
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/urisohn/sohn}
#' }
#'
#' @docType package
#' @name sohn-package
#' @aliases sohn
NULL
