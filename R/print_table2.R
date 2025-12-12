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
  
  # If we don't have dimension names, fall back to default print
  if (is.null(dim_names) || length(dim_names) != 2) {
    return(NextMethod())
  }
  
  row_var_name <- dim_names[1]
  col_var_name <- dim_names[2]
  
  # Get row and column labels
  row_labels <- dimn[[1]]
  col_labels <- dimn[[2]]
  
  # Calculate widths for formatting
  col_widths <- nchar(col_labels)
  max_col_width <- max(col_widths, na.rm = TRUE)
  row_label_width <- max(nchar(row_labels), na.rm = TRUE)
  row_var_width <- nchar(row_var_name)
  
  # Estimate column spacing (R typically uses 2 spaces between columns)
  col_spacing <- 2
  
  # Calculate total width of all columns
  n_cols <- length(col_labels)
  total_col_width <- sum(col_widths) + (n_cols - 1) * col_spacing
  
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
  for (i in seq_along(col_labels)) {
    cat(sprintf(paste0("%", max_col_width + col_spacing, "s"), col_labels[i]))
  }
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
    for (j in seq_along(col_labels)) {
      cat(sprintf(paste0("%", max_col_width + col_spacing, "s"), x[i, j]))
    }
    cat("\n")
  }
  
  cat("\n")
  
  invisible(x)
}

