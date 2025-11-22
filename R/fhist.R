#' Plot empirical distribution of a variable (histogram without binning)
#'
#' Creates a frequency plot showing the frequency of every observed value,
#' displaying the full range from minimum to maximum value.
#'
#' @param x A numeric vector of values to plot frequencies for.
#' @param col Color for the frequency bars. Default is "dodgerblue".
#' @param lwd Line width for the frequency bars. Default is 9.
#' @param value.labels Logical. If TRUE, displays frequencies on top of each line. Default is TRUE
#' @param ... Pass on any other argument that's accepted by \code{plot()}.
#'
#' @return Invisibly returns a data frame with values and their frequencies.
#'
#' @details
#' This function creates a frequency plot where each observed value is shown
#' with its frequency. Unlike a standard histogram, there is no binning, unlike
#' a barplot, non-observed values of the variable are shown with 0 frequency 
#' instead of skipped.
#'
#' @examples
#' # Simple example
#' x <- c(1, 1, 2, 2, 2, 5, 5)
#' fhist(x)
#'
#' # Pass on some common \code{plot()} arguments
#' fhist(x, col = "steelblue", xlab = "Value", ylab = "Frequency",ylim=c(0,7))
#'

#' @export
fhist <- function(x, col='dodgerblue',lwd=9, value.labels=TRUE, ...) {
  # Capture x variable name for x-axis label (before potentially overwriting)
  x_name <- deparse(substitute(x))
  
  # Extract additional arguments
  dots <- list(...)
  
  # Calculate frequencies for each unique value
  freq_table <- table(x)
  xs <- as.numeric(names(freq_table))
  fs <- as.numeric(freq_table)
  
  # Create full range from min to max
  full_range <- min(xs):max(xs)
  
  # Initialize frequencies for full range (zeros for missing values)
  fs_full <- numeric(length(full_range))
  names(fs_full) <- full_range
  
  # Fill in observed frequencies
  fs_full[as.character(xs)] <- fs
  
  # Update xs and fs to include full range
  xs <- full_range
  fs <- fs_full
  
  # Set default xlab if not provided
  if (!"xlab" %in% names(dots)) dots$xlab <- x_name
  
  # Set default ylab if not provided
  if (!"ylab" %in% names(dots)) dots$ylab <- "Frequency"
  
  # Set default main title if not provided
  if (!"main" %in% names(dots)) dots$main <- paste("Distribution of", x_name)
  
  # Set default ylim to start at 0 if not provided
  if (!"ylim" %in% names(dots)) {
    y_max <- max(fs, na.rm = TRUE)
    # Add extra space at top if value labels are shown
    if (value.labels == TRUE && y_max > 0) {
      y_max <- y_max + max(1, y_max * 0.15)  # Add 15% or at least 1 unit
    }
    dots$ylim <- c(0, y_max)
  }
  
  # Remove default axis padding to eliminate gap at bottom
  if (!"yaxs" %in% names(dots)) dots$yaxs <- "i"
  if (!"xaxs" %in% names(dots)) dots$xaxs <- "i"
  
  # Add las=1 to dots if not provided
  if (!"las" %in% names(dots)) dots$las <- 1
  
  # Set axis label formatting (xlab/ylab, not tick labels)
  if (!"font.lab" %in% names(dots)) dots$font.lab <- 2
  if (!"cex.lab" %in% names(dots)) dots$cex.lab <- 1.2
  
  # Set type='n' to create plot frame without drawing points (unless user specifies type)
  if (!"type" %in% names(dots)) dots$type <- "n"
  
  # Track if user provided yaxt - if not, we'll draw custom integer-only axis
  user_provided_yaxt <- "yaxt" %in% names(dots)
  if (!user_provided_yaxt) dots$yaxt <- "n"
  
  # Plot the frequencies (empty plot frame)
  plot_args <- c(list(x = xs, y = fs), dots)
  do.call(plot, plot_args)
  
  # Draw custom y-axis with integer ticks only (if we suppressed the default axis)
  if (!user_provided_yaxt) {
    # Use the actual ylim maximum for ticks
    y_max_actual <- dots$ylim[2]
    y_ticks <- 0:ceiling(y_max_actual)
    axis(2, at = y_ticks, las = 1)
  }
  
  # Identify non-zero frequencies (only draw segments and labels for these)
  non_zero <- fs > 0
  
  # Draw segments for each value (only non-zero frequencies)
  if (any(non_zero)) {
    segments(x0 = xs[non_zero], x1 = xs[non_zero], y0 = 0, y1 = fs[non_zero], 
             lwd = lwd, col = col, lend = 1)
  }
  
  # Add value labels if requested
  if (value.labels == TRUE && any(non_zero)) {
    text(x = xs[non_zero], y = fs[non_zero], labels = fs[non_zero], 
         cex = 0.7, pos = 3)
  }
  
  # Return frequencies invisibly
  invisible(data.frame(value = xs, frequency = fs))
}

