#' Plot GAM Model Predictions
#'
#' Creates a plot of predicted values from a GAM model for a specified predictor
#' variable, with other variables held at their quantile values (default: median).
#'
#' @param model A GAM model object fitted using \code{mgcv::gam()}.
#' @param predictor Character string specifying the name of the predictor variable
#'   to plot on the x-axis.
#' @param quantile.others Numeric value between 1 and 99 specifying the quantile
#'   at which to hold all other variables constant. Default is 50 (median).
#' @param col Color for the prediction line. Default is "blue4".
#' @param bg Background color for the confidence band. Default is
#'   \code{adjustcolor('dodgerblue', .2)}.
#' @param ... Additional arguments passed to \code{plot()} and \code{lines()}.
#'
#' @return Invisibly returns a list containing:
#' \itemize{
#'   \item \code{predictor_values}: The sequence of predictor values used
#'   \item \code{predicted}: The predicted values
#'   \item \code{se}: The standard errors
#'   \item \code{lower}: Lower confidence bound (predicted - 2*se)
#'   \item \code{upper}: Upper confidence bound (predicted + 2*se)
#' }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Validates that the model is from \code{mgcv::gam()}
#'   \item Extracts all variables from the GAM model
#'   \item Sets all variables except \code{predictor} to their specified quantile
#'     (default: median)
#'   \item Generates 100 equally spaced values for \code{predictor} between its
#'     minimum and maximum values in the model data
#'   \item Computes predicted values and standard errors using \code{predict()}
#'   \item Plots the predicted values with confidence bands (2 standard errors)
#' }
#'
#' @examples
#' \dontrun{
#' library(mgcv)
#' # Fit a GAM model
#' data(mtcars)
#' model <- gam(mpg ~ s(hp) + s(wt) + factor(cyl), data = mtcars)
#'
#' # Plot effect of hp (with other variables at median)
#' plot_gam(model, "hp")
#'
#' # Plot effect of hp (with other variables at 25th percentile)
#' plot_gam(model, "hp", quantile.others = 25)
#'
#' # Customize plot
#' plot_gam(model, "hp", main = "Effect of Horsepower", col = "blue", lwd = 2)
#' }
#'
#' @importFrom mgcv gam
#' @export
plot_gam <- function(model, predictor, quantile.others = 50, 
                     col = "blue4", bg = adjustcolor('dodgerblue', .2), ...) {
  # Check if model is from mgcv::gam()
  if (!inherits(model, "gam")) {
    stop("'model' must be a GAM model object fitted using mgcv::gam()")
  }
  
  # Check if mgcv is available
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required. Please install it with: install.packages('mgcv')")
  }
  
  # Check if factor() is used in the model formula
  # This causes issues with predict() - variables should be converted to factor before gam()
  model_formula <- formula(model)
  formula_char <- paste(deparse(model_formula), collapse = " ")
  # Check for factor( in the formula (factor is lowercase in R)
  if (grepl("\\bfactor\\s*\\(", formula_char, ignore.case = FALSE)) {
    stop("The model formula contains factor() calls. Please convert variables to factors before fitting the model.\n",
         "Example: Instead of gam(y ~ factor(x), data = df), do:\n",
         "  df$x <- factor(df$x)\n",
         "  gam(y ~ x, data = df)")
  }
  
  # Validate quantile.others
  if (!is.numeric(quantile.others) || length(quantile.others) != 1) {
    stop("'quantile.others' must be a single numeric value")
  }
  if (quantile.others < 1 || quantile.others > 99) {
    stop("'quantile.others' must be between 1 and 99")
  }
  
  # Validate predictor
  if (!is.character(predictor) || length(predictor) != 1) {
    stop("'predictor' must be a single character string")
  }
  
  # Extract model data
  # Try model$model first, then model.frame() as fallback
  model_data <- model$model
  if (is.null(model_data)) {
    # Try to get model frame from the model
    tryCatch({
      model_data <- model.frame(model)
    }, error = function(e) {
      stop("Model data not found. Please refit the model with 'keepData = TRUE' or ensure model data is available.")
    })
  }
  
  # Get the original data if available (from model call)
  # This helps ensure we have the right variable structure
  original_data <- NULL
  if (!is.null(model$call$data)) {
    tryCatch({
      original_data <- eval(model$call$data, envir = environment(formula(model)))
    }, error = function(e) {
      # If we can't get original data, use model_data
      original_data <- model_data
    })
  } else {
    original_data <- model_data
  }
  
  # Get all variable names from the model frame
  all_vars <- names(model_data)
  
  # Remove response variable (first column is typically the response)
  response_var <- all_vars[1]
  predictor_vars <- all_vars[-1]
  
  # Check if predictor exists in model data
  if (!predictor %in% predictor_vars) {
    stop(sprintf("Predictor '%s' not found in model variables. Available variables: %s",
                 predictor, paste(predictor_vars, collapse = ", ")))
  }
  
  # Get other variables (all predictors except the one we're plotting)
  other_vars <- setdiff(predictor_vars, predictor)
  
  # Create new dataset for prediction
  # Start with the predictor variable: 100 equally spaced values between min and max
  predictor_values <- model_data[[predictor]]
  predictor_min <- min(predictor_values, na.rm = TRUE)
  predictor_max <- max(predictor_values, na.rm = TRUE)
  predictor_seq <- seq(predictor_min, predictor_max, length.out = 100)
  n_rows <- length(predictor_seq)
  
  # CRITICAL: Replicate the entire model frame structure first
  # This preserves ALL attributes, factor structures, and internal mgcv requirements
  new_data <- model_data[rep(1, n_rows), , drop = FALSE]
  
  # Now modify the predictor variable
  new_data[[predictor]] <- predictor_seq
  
  # Set other variables to their quantile/default values
  for (var in other_vars) {
    # Get variable from model_data (this is what mgcv sees)
    var_values <- model_data[[var]]
    
    # Handle different variable types
    if (is.factor(var_values)) {
      # For factors, use the first/lowest factor level
      first_level <- levels(var_values)[1]
      # Find a row with this level to preserve exact factor structure
      idx <- which(var_values == first_level)[1]
      if (length(idx) == 0) idx <- 1
      # Replicate the exact factor value from model_data
      new_data[[var]] <- var_values[rep(idx, n_rows)]
    } else if (is.character(var_values)) {
      # For character vectors, use the most common value
      var_levels <- table(var_values)
      most_common <- names(var_levels)[which.max(var_levels)]
      new_data[[var]] <- rep(most_common, n_rows)
    } else if (is.numeric(var_values)) {
      # For numeric variables, use the specified quantile
      quantile_value <- quantile(var_values, probs = quantile.others / 100, na.rm = TRUE)
      new_data[[var]] <- rep(as.numeric(quantile_value), n_rows)
    } else {
      # For other types, try to use the most common value
      warning(sprintf("Variable '%s' has unsupported type. Using most common value.", var))
      var_levels <- table(var_values)
      most_common <- names(var_levels)[which.max(var_levels)]
      new_data[[var]] <- rep(most_common, n_rows)
    }
  }
  
  # Extract only predictor variables (exclude response) for prediction
  new_data <- new_data[, predictor_vars, drop = FALSE]
  
  # Make predictions with standard errors
  pred_result <- predict(model, newdata = new_data, se.fit = TRUE)
  predicted <- pred_result$fit
  se <- pred_result$se.fit
  
  # Calculate confidence bounds (2 standard errors)
  lower <- predicted - 2 * se
  upper <- predicted + 2 * se
  
  # Extract additional arguments for plotting
  dots <- list(...)
  
  # Remove col and bg from dots if present (use formal arguments instead)
  if ("col" %in% names(dots)) {
    col <- dots$col
    dots$col <- NULL
  }
  if ("bg" %in% names(dots)) {
    bg <- dots$bg
    dots$bg <- NULL
  }
  
  # Set default labels if not provided
  if (!"xlab" %in% names(dots)) dots$xlab <- predictor
  if (!"ylab" %in% names(dots)) dots$ylab <- response_var
  
  # Set default formatting
  if (!"font.lab" %in% names(dots)) dots$font.lab <- 2
  if (!"cex.lab" %in% names(dots)) dots$cex.lab <- 1.2
  if (!"las" %in% names(dots)) dots$las <- 1
  
  # Determine ylim if not provided
  if (!"ylim" %in% names(dots)) {
    dots$ylim <- range(c(lower, upper), na.rm = TRUE)
  }
  
  # Set default xlim if not provided
  if (!"xlim" %in% names(dots)) {
    dots$xlim <- range(predictor_seq, na.rm = TRUE)
  }
  
  # Plot predicted values
  plot_args <- c(list(x = predictor_seq, y = predicted, type = "l"), dots)
  do.call(plot, plot_args)
  
  # Add confidence bands
  polygon(c(predictor_seq, rev(predictor_seq)), 
          c(lower, rev(upper)),
          col = bg,
          border = NA)
  
  # Redraw the prediction line on top
  lines(predictor_seq, predicted, col = col, ...)
  
  # Return results invisibly
  result <- list(
    predictor_values = predictor_seq,
    predicted = predicted,
    se = se,
    lower = lower,
    upper = upper
  )
  
  invisible(result)
}







