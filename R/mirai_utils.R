# Mirai Utilities -----------------------------------------------------------

#' Get mirai daemon status
#'
#' Returns the current status of mirai daemons, including the number
#' of active daemons and their connection state.
#'
#' @return A list with daemon status information, or NULL if mirai is not installed
#' @export
#' @examples
#' \donttest{
#' # Check daemon status
#' status <- mirai_status()
#' if (!is.null(status)) {
#'   print(status)
#' }
#' }
mirai_status <- function() {
  if (requireNamespace("mirai", quietly = TRUE)) {
    tryCatch(
      mirai::status(),
      error = function(e) {
        message("No mirai daemons running")
        NULL
      }
    )
  } else {
    message("mirai package not installed")
    NULL
  }
}

#' Get mirai dispatcher status
#'
#' Returns detailed status information about the mirai dispatcher,
#' including queue depth and task distribution.
#'
#' @return A list with dispatcher status, or NULL if no dispatcher is running
#' @export
#' @examples
#' \donttest{
#' # Check dispatcher status
#' disp_status <- mirai_dispatcher_status()
#' if (!is.null(disp_status)) {
#'   print(disp_status)
#' }
#' }
mirai_dispatcher_status <- function() {
  if (requireNamespace("mirai", quietly = TRUE)) {
    tryCatch({
      # Try to get dispatcher status without statically referencing it
      if ("dispatcher_status" %in% getNamespaceExports("mirai")) {
        getExportedValue("mirai", "dispatcher_status")()
      } else {
        # Fall back to status if dispatcher_status not available
        mirai::status()
      }
    },
    error = function(e) {
      message("No dispatcher running")
      NULL
    })
  } else {
    message("mirai package not installed")
    NULL
  }
}

#' Scale mirai daemons
#'
#' Dynamically adjust the number of mirai daemons. Useful for
#' scaling compute resources based on workload.
#'
#' @param n New number of daemons
#' @return Invisibly returns NULL
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mirai", quietly = TRUE)) {
#'   # Scale to 8 daemons
#'   mirai_scale(8)
#' }
#' }
mirai_scale <- function(n) {
  if (requireNamespace("mirai", quietly = TRUE)) {
    current <- tryCatch(
      mirai::status(),
      error = function(e) NULL
    )
    
    if (is.null(current)) {
      message("No daemons running - initializing with ", n, " daemons")
      mirai::daemons(n = n, dispatcher = TRUE)
    } else {
      old_n <- current$daemons
      mirai::daemons(n = n, dispatcher = TRUE)
      message(sprintf("Scaled from %d to %d daemons", old_n, n))
    }
  } else {
    stop("mirai package required")
  }
  invisible(NULL)
}

#' Stop all mirai daemons
#'
#' Convenience function to cleanly shut down all mirai daemons.
#'
#' @return Invisibly returns NULL
#' @export
#' @examples
#' \donttest{
#' # Stop all daemons
#' mirai_stop()
#' }
mirai_stop <- function() {
  if (requireNamespace("mirai", quietly = TRUE)) {
    mirai::daemons(0)
    message("All mirai daemons stopped")
  } else {
    message("mirai package not installed")
  }
  invisible(NULL)
}

#' Check mirai availability
#'
#' Test whether mirai and future.mirai packages are available
#' and properly configured.
#'
#' @return Logical indicating whether mirai is available
#' @export
#' @examples
#' if (mirai_available()) {
#'   message("Mirai is available")
#' }
mirai_available <- function() {
  requireNamespace("mirai", quietly = TRUE) && 
  requireNamespace("future.mirai", quietly = TRUE)
}

#' Initialize mirai for parade
#'
#' One-step initialization of mirai for use with parade workflows.
#' Sets up daemons and configures the future plan.
#'
#' @param n Number of local daemons (defaults to number of cores)
#' @param dispatcher Use dispatcher for load balancing
#' @return Invisibly returns the previous future plan
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mirai", quietly = TRUE) && 
#'     requireNamespace("future.mirai", quietly = TRUE)) {
#'   # Initialize with default settings
#'   mirai_init()
#'   
#'   # Initialize with 4 daemons
#'   mirai_init(n = 4)
#'   
#'   # Clean up
#'   mirai_stop()
#' }
#' }
mirai_init <- function(n = NULL, dispatcher = TRUE) {
  if (!mirai_available()) {
    stop("mirai and future.mirai packages required. Install with:\n",
         "install.packages(c('mirai', 'future.mirai'))")
  }
  
  n <- n %||% parallel::detectCores()
  
  # Initialize daemons
  mirai::daemons(n = n, dispatcher = dispatcher)
  
  # Set future plan
  old_plan <- future::plan()
  future::plan(future.mirai::mirai_multisession)
  
  message(sprintf("Initialized mirai with %d daemons", n))
  invisible(old_plan)
}

# Use the %||% operator from utils.R instead of redefining
