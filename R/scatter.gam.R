#' Scatter Plot with GAM Smooth Line
#'
#' Creates a scatter plot with a GAM (Generalized Additive Model) smooth line,
#' with options to display data points and three-way spline summary points.
#'
#' @param x A numeric vector of x values.
#' @param y A numeric vector of y values.
#' @param data.dots Logical. If TRUE, displays the original data points on the
#'   plot. Default is FALSE.
#' @param three.dots Logical. If TRUE, displays three summary points representing
#'   the mean x and y values for each tertile of x. Default is FALSE.
#' @param data An optional data frame containing the variables \code{x} and \code{y}.
#' @param k Optional integer specifying the basis dimension for the smooth term
#'   in the GAM model (passed to \code{s(x, k=k)}). If NULL (default), uses the
#'   default basis dimension.
#' @param ... Additional arguments passed to \code{plot()} and \code{gam()}.
#'
#' @return Invisibly returns the fitted GAM model object.
#'
#' @details
#' This function fits a GAM model with a smooth term for x and plots the fitted
#' smooth line. The function uses the \code{mgcv} package's \code{gam()} function.
#'
#' When \code{three.dots = TRUE}, the x variable is divided into three equal-sized
#' groups (tertiles), and the mean x and y values for each group are plotted as
#' points. This provides a simple summary of the relationship across the range of x.
#'
#' @examples
#' # Basic usage
#' x <- rnorm(100)
#' y <- 2*x + rnorm(100)
#' scatter.gam(x, y)
#'
#' # With data points
#' scatter.gam(x, y, data.dots = TRUE)
#'
#' # With three-way spline points
#' scatter.gam(x, y, three.dots = TRUE)
#'
#' # Both options
#' scatter.gam(x, y, data.dots = TRUE, three.dots = TRUE)
#'
#' # Using data frame
#' df <- data.frame(x = rnorm(100), y = 2*rnorm(100) + rnorm(100))
#' scatter.gam(x, y, data = df, data.dots = TRUE)
#'
#' # Custom styling
#' scatter.gam(x, y, data.dots = TRUE, col = "red", lwd = 2, main = "GAM Fit")
#'
#' # With custom basis dimension
#' scatter.gam(x, y, k = 10)
#'
#' @importFrom mgcv gam
#' @export
scatter.gam <- function(x, y, data.dots = FALSE, three.dots = FALSE, data = NULL, k = NULL, ...) {
  # Capture x and y names for labels (before potentially overwriting)
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  # Extract additional arguments
  dots <- list(...)
  
  # Handle data frame if provided
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame")
    }
    
    # Extract columns from data frame
    if (!x_name %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", x_name))
    }
    if (!y_name %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", y_name))
    }
    
    x <- data[[x_name]]
    y <- data[[y_name]]
  }
  
  # Check for required package
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required. Please install it with: install.packages('mgcv')")
  }
  
  # Fit GAM model
  # Define GAM argument names (to avoid duplication)
  gam_arg_names <- c("family", "method", "optimizer", "control", 
                     "scale", "select", "knots", "sp", "min.sp",
                     "H", "gamma", "fit", "paraPen", "G", "in.out",
                     "drop.unused.levels", "drop.intercept", "nthreads",
                     "cluster", "mustart", "etastart", "offset", "subset",
                     "na.action", "start", "model", "x", "y")
  
  # Separate gam arguments from plot arguments
  gam_args <- dots[names(dots) %in% gam_arg_names]
  plot_args <- dots[!names(dots) %in% gam_arg_names]
  
  # Build GAM formula with optional k parameter
  if (!is.null(k)) {
    gam_formula <- y ~ s(x, k = k)
  } else {
    gam_formula <- y ~ s(x)
  }
  g1 <- do.call(mgcv::gam, c(list(formula = gam_formula), gam_args))
  
  # Predict fitted values
  yh <- predict(g1)
  
  # Order by x for smooth line plotting
  ord <- order(x)
  
  # Calculate three-way spline points if requested
  if (three.dots == TRUE) {
    xq <- cut(x, 3)
    # aggregate returns data frame with Group.1 (factor levels) and x (aggregated values)
    x3_agg <- aggregate(x, list(xq), mean, na.rm = TRUE)
    y3_agg <- aggregate(y, list(xq), mean, na.rm = TRUE)
    x3_means <- x3_agg$x  # Mean x values for each tertile
    y3_means <- y3_agg$x  # Mean y values for each tertile
  }
  
  # Determine ylim
  if (data.dots == TRUE && three.dots == TRUE) {
    ylim <- range(c(y, yh, y3_means), na.rm = TRUE)
  } else if (data.dots == TRUE) {
    ylim <- range(y, na.rm = TRUE)
  } else if (three.dots == TRUE) {
    ylim <- range(c(yh, y3_means), na.rm = TRUE)
  } else {
    ylim <- range(yh, na.rm = TRUE)
  }
  
  # Set default labels if not provided
  if (!"xlab" %in% names(plot_args)) plot_args$xlab <- x_name
  if (!"ylab" %in% names(plot_args)) plot_args$ylab <- y_name
  
  # Set axis label formatting and orientation
  if (!"font.lab" %in% names(plot_args)) plot_args$font.lab <- 2
  if (!"cex.lab" %in% names(plot_args)) plot_args$cex.lab <- 1.2
  if (!"las" %in% names(plot_args)) plot_args$las <- 1
  
  # Plot smooth line
  plot_args_line <- c(list(x = x[ord], y = yh[ord], type = 'l', col = 'blue', ylim = ylim), 
                      plot_args)
  do.call(plot, plot_args_line)
  
  # Add data points if requested
  if (data.dots == TRUE) {
    points(x, y, col = 'gray66')
  }
  
  # Add three-way spline points if requested
  if (three.dots == TRUE) {
    points(x3_means, y3_means, pch = 16)
  }
  
  # Return GAM model invisibly
  invisible(g1)
}

