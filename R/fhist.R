#' Plot empirical distribution of a variable (histogram without binning)
#'
#' Creates a frequency plot showing the frequency of every observed value,
#' displaying the full range from minimum to maximum value.
#'
#' @param x A numeric vector of values to plot frequencies for.
#' @param col Color for the frequency bars. Default is "dodgerblue".
#' @param lwd Line width for the frequency bars. Default is 9.
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
fhist <- function(x, col='dodgerblue',lwd=9,...) {
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
  
  # Plot the frequencies
  plot(xs, fs, ...)
  
  # Draw segments for each value
  segments(x0 = xs, x1 = xs, y0 = 0, y1 = fs, lwd = lwd, col = col)
  
  # Return frequencies invisibly
  invisible(data.frame(value = xs, frequency = fs))
}

