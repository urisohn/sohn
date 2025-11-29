#' Simulation Counter
#'
#' Function that adaptively finds the frequency with which to report simulation
#' progress so that it reports approximately within 5 seconds.
#'
#' @param simk The current iteration number
#'
#' @return Prints progress updates (invisibly returns NULL)
#'
#' @details
#' This function:
#' \itemize{
#'   \item Monitors execution time at checkpoints (iterations 2, 5, 10, 50)
#'   \item Automatically determines an optimal reporting interval based on a
#'     target time (5 seconds by default)
#'   \item Prints progress updates at the chosen interval
#'   \item Adapts to fast or slow iterations
#' }
#'
#' The reporter works in two phases:
#' \itemize{
#'   \item \strong{Discovery phase}: Checks execution time at checkpoints. If
#'     any checkpoint takes longer than the target time, it sets the reporting
#'     interval to that checkpoint value.
#'   \item \strong{Reporting phase}: Once an interval is chosen, it reports
#'     progress every N iterations, where N is the chosen interval.
#' }
#'
#' If all checkpoints complete quickly (before reaching iteration 50), the
#' function estimates an appropriate interval based on the average time per
#' iteration.
#'
#' State (simk and time) is tracked within the function's environment.
#'
#' @examples
#' # Use it in a simulation loop
#' for (i in 1:100) {
#'   # Your simulation code here
#'   Sys.sleep(0.1)  # Simulate work
#'   counter(i)  # Report progress
#' }
#'
#' # The reporter will automatically determine when to print updates
#' # based on how long each iteration takes
#'
#' @export
counter <- function(simk) {
  # Initialize state in function's environment if first call
  if (!exists("chosen_int", envir = environment(counter))) {
    assign("checkpoints", c(2, 5, 10, 50), envir = environment(counter))
    assign("target_sec", 5, envir = environment(counter))
    assign("chosen_int", NULL, envir = environment(counter))
    assign("t_start", proc.time()[3], envir = environment(counter))
  }
  
  # Get current state
  checkpoints <- get("checkpoints", envir = environment(counter))
  target_sec <- get("target_sec", envir = environment(counter))
  chosen_int <- get("chosen_int", envir = environment(counter))
  t_start <- get("t_start", envir = environment(counter))
  
  # If interval already chosen
  if (!is.null(chosen_int)) {
    if (simk %% chosen_int == 0) cat("sim", simk, "\n")
    return(invisible(NULL))
  }

  # We're still in the discovery phase
  if (simk %in% checkpoints) {
    elapsed <- proc.time()[3] - t_start
    if (elapsed >= target_sec) {
      assign("chosen_int", simk, envir = environment(counter))
      cat("[set interval =", simk, "] sim", simk, "\n")
      return(invisible(NULL))
    }
  }

  # At simk = 50 we decide if none were slow
  if (simk == 50) {
    elapsed <- proc.time()[3] - t_start
    # estimated cost per iteration
    per_iter <- elapsed / simk
    interval <- max(1, round(target_sec / per_iter))
    assign("chosen_int", interval, envir = environment(counter))
    #cat("[estimated interval =", interval, "] sim", simk, "\n")
    return(invisible(NULL))
  }

  # Default: print only discovery checkpoints
  if (simk %in% checkpoints) cat("sim", simk, "\n")
  
  invisible(NULL)
}
