#' Plot Empirical Cumulative Distribution Functions by Group
#'
#' Plots empirical cumulative distribution functions (ECDFs) separately for
#' each unique value of a grouping variable, with support for vectorized
#' plotting parameters.
#'
#' @param y A numeric vector of values to compute ECDFs for, or a column name
#'   if \code{data} is provided.
#' @param x A vector (factor, character, or numeric) used to group the data,
#'   or a column name if \code{data} is provided.
#' @param data An optional data frame containing the variables \code{y} and \code{x}.
#' @param ... Additional arguments passed to plotting functions. Can be scalars
#'   (applied to all groups) or vectors (applied element-wise to each group).
#'   Common parameters include \code{col}, \code{lwd}, \code{lty}, \code{pch},
#'   \code{type}, etc.
#'
#' @return Invisibly returns a list of ECDF functions, one for each group.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Splits \code{y} by unique values of \code{x}
#'   \item Computes an ECDF for each group
#'   \item Plots all ECDFs on the same graph
#'   \item Handles plotting parameters: scalars apply to all groups, vectors
#'     apply element-wise to groups (in order of unique \code{x} values)
#' }
#'
#' The ECDFs are plotted as step functions with vertical lines. Parameters like
#' \code{col}, \code{lwd}, \code{lty}, and \code{pch} can be specified as:
#' \itemize{
#'   \item A single value: applied to all groups
#'   \item A vector: applied to groups in order of unique \code{x} values
#' }
#'
#' @examples
#' # Basic usage
#' y <- rnorm(100)
#' x <- rep(c("A", "B", "C"), c(30, 40, 30))
#' cdf.by(y, x)
#'
#' # With custom colors (scalar - same for all)
#' cdf.by(y, x, col = "blue")
#'
#' # With custom colors (vector - different for each group)
#' cdf.by(y, x, col = c("red", "green", "blue"))
#'
#' # Multiple parameters
#' cdf.by(y, x, col = c("red", "green", "blue"), lwd = c(1, 2, 3))
#'
#' # With line type and point character
#' cdf.by(y, x, col = c("red", "green", "blue"), lty = c(1, 2, 3), lwd = 2)
#'
#' # Using data frame
#' df <- data.frame(value = rnorm(100), group = rep(c("A", "B"), 50))
#' cdf.by(value, group, data = df)
#' cdf.by(value, group, data = df, col = c("red", "blue"))
#'
#' @export
cdf.by <- function(y, x, data = NULL, ...) {
  # Capture y name for xlab (before potentially overwriting y)
  y_name <- deparse(substitute(y))
  
  # Extract plotting parameters from ...
  dots <- list(...)
  
  # Handle data frame if provided
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame")
    }
    
    # Extract columns from data frame
    if (!y_name %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", y_name))
    }
    x_name <- deparse(substitute(x))
    if (!x_name %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", x_name))
    }
    
    y <- data[[y_name]]
    x <- data[[x_name]]
  }
  
  # Get unique groups and their order
  unique_x <- unique(x)
  n_groups <- length(unique_x)
  
  # Helper function to extract parameter value for a group
  get_param <- function(param_name, group_idx) {
    if (param_name %in% names(dots)) {
      param_val <- dots[[param_name]]
      if (length(param_val) == 1) {
        return(param_val)
      } else if (length(param_val) >= group_idx) {
        return(param_val[group_idx])
      } else {
        return(param_val[1])  # Recycle if shorter
      }
    }
    return(NULL)
  }
  
  # Compute ECDFs for each group
  ecdf_list <- list()
  y_ranges <- list()
  
  for (i in seq_along(unique_x)) {
    group_val <- unique_x[i]
    y_group <- y[x == group_val]
    if (length(y_group) > 0) {
      ecdf_list[[i]] <- ecdf(y_group)
      y_ranges[[i]] <- range(y_group)
    }
  }
  
  # Determine overall range for plotting
  all_y <- unlist(y_ranges)
  y_min <- min(all_y, na.rm = TRUE)
  y_max <- max(all_y, na.rm = TRUE)
  y_range <- y_max - y_min
  y_lim <- c(y_min - 0.05 * y_range, y_max + 0.05 * y_range)
  
  # Create sequence for plotting ECDF
  y_seq <- seq(y_min, y_max, length.out = 1000)
  
  # Helper function for NULL coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Plot first ECDF to set up the plot
  if (length(ecdf_list) > 0) {
    first_ecdf <- ecdf_list[[1]]
    first_y_vals <- first_ecdf(y_seq)
    
    # Get parameters for first group
    col1 <- get_param("col", 1) %||% 1
    lwd1 <- get_param("lwd", 1) %||% 1
    lty1 <- get_param("lty", 1) %||% 1
    type1 <- get_param("type", 1) %||% "s"
    pch1 <- get_param("pch", 1)
    
    # Remove vectorized parameters and data from dots for plot() (keep others like xlab, ylab, etc.)
    plot_dots <- dots
    vectorized_params <- c("col", "lwd", "lty", "type", "pch", "data")
    plot_dots[vectorized_params] <- NULL
    
    # Build plot arguments
    plot_args <- list(x = y_seq, y = first_y_vals, 
                      type = type1, col = col1, lwd = lwd1, lty = lty1,
                      xlab = y_name, 
                      ylab = "Cumulative Probability",
                      ylim = c(0, 1))
    if (!is.null(pch1)) plot_args$pch <- pch1
    
    # Set up plot
    do.call(plot, c(plot_args, plot_dots))
    
    # Add remaining ECDFs
    if (length(ecdf_list) > 1) {
      for (i in 2:length(ecdf_list)) {
        ecdf_fun <- ecdf_list[[i]]
        y_vals <- ecdf_fun(y_seq)
        
        # Get parameters for this group
        coli <- get_param("col", i) %||% i
        lwdi <- get_param("lwd", i) %||% 1
        ltyi <- get_param("lty", i) %||% 1
        typei <- get_param("type", i) %||% "s"
        pchi <- get_param("pch", i)
        
        # Build lines arguments
        lines_args <- list(x = y_seq, y = y_vals, type = typei, 
                          col = coli, lwd = lwdi, lty = ltyi)
        if (!is.null(pchi)) lines_args$pch <- pchi
        
        do.call(lines, lines_args)
      }
    }
  }
  
  # Return ECDFs invisibly
  names(ecdf_list) <- as.character(unique_x)
  invisible(ecdf_list)
}

