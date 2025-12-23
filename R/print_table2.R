#' Print method for table2 output with centered column variable name
#'
#' @param x An object of class \code{table2}
#' @param ... Additional arguments passed to print
#'
#' @export
print.table2 <- function(x, ...) {
  # Get dimension names
  dimn <- dimnames(x)
  dim_names <- names(dimn)
  
  # Check if this is a proportion table
  is_proportion <- isTRUE(attr(x, "is_proportion"))
  
  # If we don't have dimension names and it's not a proportion table, fall back to default print
  if ((is.null(dim_names) || length(dim_names) != 2) && !is_proportion) {
    return(NextMethod())
  }
  
  # For proportion tables without dimension names, use default print but with custom formatting
  if (is.null(dim_names) || length(dim_names) != 2) {
    # Still use NextMethod but the formatting should be handled by the proportion logic
    # Actually, we need dimension names to format properly, so fall back
    return(NextMethod())
  }
  
  row_var_name <- dim_names[1]
  col_var_name <- dim_names[2]
  
  # Get row and column labels
  row_labels <- dimn[[1]]
  col_labels <- dimn[[2]]
  
  # Calculate widths for formatting
  # First, calculate width of column labels
  col_label_widths <- nchar(col_labels)
  max_col_label_width <- max(col_label_widths, na.rm = TRUE)
  
  # Check if this is a proportion table
  is_proportion <- isTRUE(attr(x, "is_proportion"))
  proportion_digits <- attr(x, "proportion_digits")
  if (is.null(proportion_digits)) proportion_digits <- 3
  
  # Calculate width of actual data values - format each value and check width
  max_data_width <- 0
  for (i in seq_along(row_labels)) {
    for (j in seq_along(col_labels)) {
      val <- x[i, j]
      if (is_proportion) {
        # Format as proportion without leading 0 (e.g., .100 instead of 0.100)
        # Always show the specified number of decimals, even for 0 (.000)
        val_rounded <- round(val * (10^proportion_digits))
        val_str <- sprintf(paste0(".%0", proportion_digits, "d"), val_rounded)
      } else {
        val_str <- as.character(val)
      }
      max_data_width <- max(max_data_width, nchar(val_str), na.rm = TRUE)
    }
  }
  
  # Use the maximum of label width and data width for column formatting
  max_col_width <- max(max_col_label_width, max_data_width)
  
  row_label_width <- max(nchar(row_labels), na.rm = TRUE)
  row_var_width <- nchar(row_var_name)
  
  # Column spacing (R typically uses 2 spaces between columns)
  col_spacing <- 2
  
  # Calculate total width of all columns
  n_cols <- length(col_labels)
  total_col_width <- n_cols * max_col_width + (n_cols - 1) * col_spacing
  
  # Calculate spacing to center column variable name over the data columns
  col_var_width <- nchar(col_var_name)
  col_var_spacing <- max(0, floor((total_col_width - col_var_width) / 2))
  
  # Total width needed for row label area (variable name + space + label + spacing)
  total_row_label_width <- row_var_width + 1 + row_label_width + col_spacing
  
  # Print column variable name centered over columns
  cat("\n")
  cat(strrep(" ", total_row_label_width + col_var_spacing))
  cat(col_var_name)
  cat("\n")
  
  # Print column labels
  cat(strrep(" ", total_row_label_width))
  # Build column labels as formatted values
  col_label_values <- character(length(col_labels))
  for (i in seq_along(col_labels)) {
    col_label_values[i] <- sprintf(paste0("%", max_col_width, "s"), col_labels[i])
  }
  # Join with spacing and print
  spacing_str <- strrep(" ", col_spacing)
  cat(paste(col_label_values, collapse = spacing_str))
  cat("\n")
  
  # Print rows with row variable name on left margin
  for (i in seq_along(row_labels)) {
    # Print row variable name and label on first row, or just label with spacing for alignment
    if (i == 1) {
      cat(sprintf(paste0("%-", row_var_width, "s"), row_var_name))
      cat(" ")
      cat(sprintf(paste0("%-", row_label_width + col_spacing, "s"), row_labels[i]))
    } else {
      cat(strrep(" ", row_var_width + 1))
      cat(sprintf(paste0("%-", row_label_width + col_spacing, "s"), row_labels[i]))
    }
    
    # Print values (right-aligned like R does)
    # Build the row as a vector of formatted values
    row_values <- character(length(col_labels))
    for (j in seq_along(col_labels)) {
      val <- x[i, j]
      if (is_proportion) {
        # Format as proportion without leading 0 (e.g., .100 instead of 0.100)
        # Always show the specified number of decimals, even for 0 (.000)
        val_rounded <- round(val * (10^proportion_digits))
        val_str <- sprintf(paste0(".%0", proportion_digits, "d"), val_rounded)
      } else {
        val_str <- as.character(val)
      }
      # Right-align the value within max_col_width
      row_values[j] <- sprintf(paste0("%", max_col_width, "s"), val_str)
    }
    # Join with spacing and print
    spacing_str <- strrep(" ", col_spacing)
    cat(paste(row_values, collapse = spacing_str))
    cat("\n")
  }
  
  cat("\n")
  
  invisible(x)
}

