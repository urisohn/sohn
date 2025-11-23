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
#' @return Invisibly returns a list containing:
#' \itemize{
#'   \item \code{ecdfs}: A list of ECDF functions, one for each group
#'   \item \code{ks_test}: (when 2 groups) The Kolmogorov-Smirnov test result
#'   \item \code{quantile_regression_25}: (when 2 groups and quantreg available) Quantile regression model for tau = 0.25
#'   \item \code{quantile_regression_50}: (when 2 groups and quantreg available) Quantile regression model for tau = 0.50
#'   \item \code{quantile_regression_75}: (when 2 groups and quantreg available) Quantile regression model for tau = 0.75
#' }
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
#' When there are exactly 2 groups, the function automatically performs:
#' \itemize{
#'   \item Kolmogorov-Smirnov test for distribution equality
#'   \item Quantile regression tests at 25th, 50th, and 75th percentiles (requires \code{quantreg} package)
#'   \item Displays test results in the bottom right corner
#'   \item Adds vertical dashed lines at the 25th, 50th, and 75th percentiles
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
  # Capture x name for legend title (before potentially overwriting x)
  x_name <- deparse(substitute(x))
  
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
    if (!x_name %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", x_name))
    }
    
    y <- data[[y_name]]
    x <- data[[x_name]]
  }
  
  # Get unique groups and their order
  unique_x <- unique(x)
  n_groups <- length(unique_x)
  
  # Initialize return values for tests (when 2 groups)
  ks_test_result <- NULL
  quantile_regression_25 <- NULL
  quantile_regression_50 <- NULL
  quantile_regression_75 <- NULL
  
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
    lwd1 <- get_param("lwd", 1) %||% 4  # Default lwd=4
    lty1 <- get_param("lty", 1) %||% 1
    type1 <- get_param("type", 1) %||% "s"
    pch1 <- get_param("pch", 1)
    
    # Remove vectorized parameters and data from dots for plot() (keep others like xlab, ylab, etc.)
    plot_dots <- dots
    vectorized_params <- c("col", "lwd", "lty", "type", "pch", "data")
    plot_dots[vectorized_params] <- NULL
    
    # Build plot arguments
    # Set main title if not provided
    if (!"main" %in% names(plot_dots)) {
      plot_dots$main <- paste0("Comparing Distribution of ", y_name, " by ", x_name)
    }
    # Set font and size for main title if not provided
    if (!"font.main" %in% names(plot_dots)) plot_dots$font.main <- 2
    if (!"cex.main" %in% names(plot_dots)) plot_dots$cex.main <- 1.3
    
    # Set default ylim if not provided (extend to 1.15 to accommodate legend above plot)
    if (!"ylim" %in% names(plot_dots)) {
      default_ylim <- c(0, 1.15)
    } else {
      default_ylim <- plot_dots$ylim
    }
    
    plot_args <- list(x = y_seq, y = first_y_vals, 
                      type = type1, col = col1, lwd = lwd1, lty = lty1,
                      xlab = y_name, 
                      ylab = "% of observations",
                      ylim = default_ylim,
                      font.lab = 2, cex.lab = 1.2, las = 1,
                      yaxt = "n")  # Suppress default y-axis to draw custom percentage axis
    if (!is.null(pch1)) plot_args$pch <- pch1
    
    # Set up plot
    do.call(plot, c(plot_args, plot_dots))
    
    # Draw custom y-axis with percentage labels
    y_ticks <- seq(0, 1, by = 0.25)
    y_labels <- paste0(y_ticks * 100, "%")
    axis(2, at = y_ticks, labels = y_labels, las = 1)
    
    # Add remaining ECDFs
    if (length(ecdf_list) > 1) {
      for (i in 2:length(ecdf_list)) {
        ecdf_fun <- ecdf_list[[i]]
        y_vals <- ecdf_fun(y_seq)
        
        # Get parameters for this group
        coli <- get_param("col", i) %||% i
        lwdi <- get_param("lwd", i) %||% 4  # Default lwd=4
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
    
    # Add legend on top with title showing x variable name
    legend_cols <- sapply(1:length(ecdf_list), function(i) get_param("col", i) %||% i)
    legend_lwds <- sapply(1:length(ecdf_list), function(i) get_param("lwd", i) %||% 4)
    legend_ltys <- sapply(1:length(ecdf_list), function(i) get_param("lty", i) %||% 1)
    legend("top", legend = as.character(unique_x), 
           col = legend_cols, lwd = legend_lwds, lty = legend_ltys,
           horiz = TRUE, bty = "n", title = x_name)
    
    # If exactly 2 groups, perform KS test and quantile regression tests
    if (n_groups == 2) {
      # Get data for both groups
      y1 <- y[x == unique_x[1]]
      y2 <- y[x == unique_x[2]]
      
      # Kolmogorov-Smirnov test
      ks_test <- ks.test(y1, y2)
      ks_test_result <- ks_test
      ks_d <- round(ks_test$statistic, 3)
      ks_p <- sohn::format.pvalue(ks_test$p.value, include_p = TRUE)
      
      # Add horizontal lines at 25%, 50%, and 75% of cumulative probability
      quantile_probs <- c(0.25, 0.50, 0.75)
      abline(h = quantile_probs, lty = 2, col = "gray80")
      
      # Quantile regression tests at 25th, 50th, and 75th percentiles
      if (requireNamespace("quantreg", quietly = TRUE)) {
        # Show message about independence assumption (only once per session)
        if (is.null(getOption("sohn.cdf.by.message.shown"))) {
          message("The p-values are done with quantile regressions that assume all observations are independent")
          options(sohn.cdf.by.message.shown = TRUE)
        }
        
        # Create data frame for quantile regression
        df_qr <- data.frame(y = y, x_group = as.numeric(x == unique_x[2]))
        
        # Calculate quantiles for each group
        q1_25 <- quantile(y1, probs = 0.25, na.rm = TRUE)
        q1_50 <- quantile(y1, probs = 0.50, na.rm = TRUE)
        q1_75 <- quantile(y1, probs = 0.75, na.rm = TRUE)
        q2_25 <- quantile(y2, probs = 0.25, na.rm = TRUE)
        q2_50 <- quantile(y2, probs = 0.50, na.rm = TRUE)
        q2_75 <- quantile(y2, probs = 0.75, na.rm = TRUE)
        
        # Get ECDF functions for finding intersections
        ecdf1 <- ecdf_list[[1]]
        ecdf2 <- ecdf_list[[2]]
        
        # Test quantiles and get p-values, store models as separate objects
        quantile_pvals <- character(length(quantile_probs))
        quantile_regression_25 <- NULL
        quantile_regression_50 <- NULL
        quantile_regression_75 <- NULL
        
        for (i in seq_along(quantile_probs)) {
          tau <- quantile_probs[i]
          tryCatch({
            qr_model <- quantreg::rq(y ~ x_group, data = df_qr, tau = tau)
            qr_summary <- summary(qr_model, se = "iid")
            qr_p <- qr_summary$coefficients[2, 4]  # p-value for x_group coefficient
            quantile_pvals[i] <- sohn::format.pvalue(qr_p, include_p = TRUE)
            
            # Store model with appropriate name
            if (tau == 0.25) {
              quantile_regression_25 <- qr_model
            } else if (tau == 0.50) {
              quantile_regression_50 <- qr_model
            } else if (tau == 0.75) {
              quantile_regression_75 <- qr_model
            }
          }, error = function(e) {
            quantile_pvals[i] <- "NA"
          })
        }
        
        # Get plot boundaries
        usr <- par("usr")
        x_range <- usr[2] - usr[1]
        y_range <- usr[4] - usr[3]
        
        # Add p-values near left border (offset upward, moved 2% to the right)
        for (i in seq_along(quantile_probs)) {
          text(x = usr[1] + 0.02 * x_range, y = quantile_probs[i] + 0.02, 
               labels = quantile_pvals[i],
               adj = c(0, 0.5), cex = 0.8, font = 2)
        }
        
        # Add value labels next to CDF lines (offset upward)
        # Group 1 (left CDF) - values to the left of the line
        text(x = q1_25, y = 0.25 + 0.02, labels = round(q1_25, 2),
             adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
        text(x = q1_50, y = 0.50 + 0.02, labels = round(q1_50, 2),
             adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
        text(x = q1_75, y = 0.75 + 0.02, labels = round(q1_75, 2),
             adj = c(1, 0.5), cex = 0.8, font = 2, col = legend_cols[1])
        
        # Group 2 (right CDF) - values to the right of the line (further right to avoid overlap)
        # Calculate offset based on plot width to move labels further right
        label_offset <- 0.03 * x_range
        text(x = q2_25 + label_offset, y = 0.25 + 0.02, labels = round(q2_25, 2),
             adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
        text(x = q2_50 + label_offset, y = 0.50 + 0.02, labels = round(q2_50, 2),
             adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
        text(x = q2_75 + label_offset, y = 0.75 + 0.02, labels = round(q2_75, 2),
             adj = c(0, 0.5), cex = 0.8, font = 2, col = legend_cols[2])
      }
      
      # Add KS test text (title in bold, D and p not bold)
      usr <- par("usr")
      x_range <- usr[2] - usr[1]
      y_range <- usr[4] - usr[3]
      # Calculate line height for positioning
      line_height <- strheight("M", cex = 0.8) * 1.2
      # Draw title in bold
      text(x = usr[2] - 0.02 * x_range, y = usr[3] + 0.02 * y_range + 2 * line_height, 
           labels = "Kolmogorov-Smirnov",
           adj = c(1, 0), cex = 0.8, font = 2)
      # Draw D and p values not bold
      ks_values <- paste0("D=", ks_d, "\n", ks_p)
      text(x = usr[2] - 0.02 * x_range, y = usr[3] + 0.02 * y_range, 
           labels = ks_values,
           adj = c(1, 0), cex = 0.8, font = 1)
    }
  }
  
  # Return ECDFs and test results
  names(ecdf_list) <- as.character(unique_x)
  
  # Build return list
  result <- list(ecdfs = ecdf_list)
  
  # Add test results if 2 groups
  if (n_groups == 2) {
    result$ks_test <- ks_test_result
    # Add quantile regression models as separate named objects
    if (!is.null(quantile_regression_25)) {
      result$quantile_regression_25 <- quantile_regression_25
    }
    if (!is.null(quantile_regression_50)) {
      result$quantile_regression_50 <- quantile_regression_50
    }
    if (!is.null(quantile_regression_75)) {
      result$quantile_regression_75 <- quantile_regression_75
    }
  }
  
  invisible(result)
}

