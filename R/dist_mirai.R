# Mirai Distribution Backend ------------------------------------------------

#' Create mirai distribution specification
#'
#' Configure distributed execution using mirai, supporting local daemons,
#' SSH tunneling, and SLURM-managed daemon pools. Mirai provides low-latency
#' task execution without R's connection limits.
#'
#' @param n Number of local daemons to launch
#' @param url Listening URL for remote connections (auto-generated if NULL)
#' @param remote Remote configuration from `mirai::ssh_config()` or `mirai::cluster_config()`
#' @param dispatcher Use dispatcher for automatic load balancing
#' @param tls Use TLS encryption for connections
#' @param port Port number for connections (auto-selected if NULL)
#' @param stop_on_exit Automatically cleanup daemons when finished
#' @param within Execution strategy within each job: "mirai" or "sequential"
#' @param workers_within Number of workers for nested parallelization
#' @param chunks_per_job Number of groups to process per job
#' @param by Column names to group by for parallelization
#' @return A `parade_dist` object for mirai execution
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mirai", quietly = TRUE)) {
#'   # Local daemons
#'   dist_mirai(n = 4)
#'   
#'   # With TLS encryption
#'   dist_mirai(n = 8, tls = TRUE, port = 5555)
#' }
#' }
#' 
#' # SSH remotes (requires configuration)
#' \donttest{
#' dist_mirai(
#'   remote = quote(mirai::ssh_config(c("ssh://node1", "ssh://node2"))),
#'   dispatcher = TRUE
#' )
#' }
dist_mirai <- function(
  n = NULL,
  url = NULL,
  remote = NULL,
  dispatcher = TRUE,
  tls = FALSE,
  port = NULL,
  stop_on_exit = TRUE,
  within = c("mirai", "sequential"),
  workers_within = NULL,
  chunks_per_job = 1L,
  by = NULL
) {
  within <- match.arg(within)
  
  # Validate inputs
  if (is.null(n) && is.null(remote)) {
    stop("dist_mirai requires either 'n' for local daemons or 'remote' for distributed execution")
  }
  
  if (!is.null(n) && !is.null(remote)) {
    stop("dist_mirai: specify either 'n' (local) or 'remote' (distributed), not both")
  }
  
  structure(
    list(
      backend = "mirai",
      by = by %||% character(),
      n = n,
      url = url,
      remote = remote,
      dispatcher = dispatcher,
      tls = tls,
      port = port,
      stop_on_exit = stop_on_exit,
      within = within,
      workers_within = workers_within,
      chunks_per_job = as.integer(chunks_per_job)
    ),
    class = "parade_dist"
  )
}

#' Use local mirai daemons
#'
#' Helper function to quickly set up local mirai daemons with
#' automatic core detection.
#'
#' @param n Number of daemons (defaults to number of CPU cores)
#' @param dispatcher Use dispatcher for load balancing
#' @return A `parade_dist` object configured for local mirai execution
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mirai", quietly = TRUE)) {
#'   # Use all available cores
#'   use_mirai_local()
#'   
#'   # Use specific number of daemons
#'   use_mirai_local(n = 4)
#' }
#' }
use_mirai_local <- function(n = NULL, dispatcher = TRUE) {
  n <- n %||% parallel::detectCores()
  dist_mirai(n = n, dispatcher = dispatcher)
}

#' Use SLURM-managed mirai daemons
#'
#' Helper function to launch mirai daemons through SLURM, ensuring
#' compliance with HPC policies while maintaining low-latency execution.
#'
#' @param n Number of daemon jobs to submit
#' @param partition SLURM partition name
#' @param time Wall time limit (e.g., "2:00:00")
#' @param mem Memory limit (e.g., "32G")
#' @param cpus CPUs per task
#' @param tls Use TLS encryption
#' @param port Port for TLS connections
#' @param ... Additional SLURM options as name=value pairs
#' @return A `parade_dist` object configured for SLURM-managed mirai
#' @export
#' @examples
#' \donttest{
#' # Launch 16 daemons on compute partition
#' use_mirai_slurm(
#'   n = 16,
#'   partition = "compute",
#'   time = "2:00:00",
#'   mem = "32G",
#'   cpus = 4
#' )
#' }
use_mirai_slurm <- function(n, partition = NULL, time = NULL, 
                            mem = NULL, cpus = NULL, tls = TRUE, 
                            port = 5555, ...) {
  # Build SLURM options string
  opts <- c()
  if (!is.null(partition)) opts <- c(opts, sprintf("#SBATCH --partition=%s", partition))
  if (!is.null(time)) opts <- c(opts, sprintf("#SBATCH --time=%s", time))
  if (!is.null(mem)) opts <- c(opts, sprintf("#SBATCH --mem=%s", mem))
  if (!is.null(cpus)) opts <- c(opts, sprintf("#SBATCH --cpus-per-task=%s", cpus))
  opts <- c(opts, "#SBATCH --job-name=parade-mirai")
  
  # Additional SLURM options
  dots <- list(...)
  for (nm in names(dots)) {
    opts <- c(opts, sprintf("#SBATCH --%s=%s", nm, dots[[nm]]))
  }
  
  opts_string <- paste(opts, collapse = "\n")
  
  # Create cluster config as quoted expression
  # This will be evaluated at runtime when mirai is available
  remote_config <- substitute(
    mirai::cluster_config(command = "sbatch", options = opts_string),
    list(opts_string = opts_string)
  )
  
  # Build URL configuration
  url_config <- if (tls) {
    substitute(
      mirai::host_url(tls = TRUE, port = port),
      list(port = port)
    )
  } else {
    NULL
  }
  
  dist_mirai(
    remote = remote_config,
    url = url_config,
    dispatcher = TRUE,
    tls = tls,
    port = port
  )
}

#' Use SSH-tunneled mirai daemons
#'
#' Helper function to connect to remote machines via SSH, with
#' optional tunneling for firewalled environments.
#'
#' @param remotes Character vector of SSH URLs (e.g., "ssh://user@host")
#' @param tunnel Use SSH tunneling for firewalled nodes
#' @param port Port for tunneled connections
#' @return A `parade_dist` object configured for SSH remotes
#' @export
#' @examples
#' \donttest{
#' # Connect through SSH tunnel
#' use_mirai_ssh(
#'   remotes = c("ssh://node1", "ssh://node2"),
#'   tunnel = TRUE
#' )
#' 
#' # Direct SSH without tunneling
#' use_mirai_ssh(
#'   remotes = c("ssh://compute1", "ssh://compute2"),
#'   tunnel = FALSE
#' )
#' }
use_mirai_ssh <- function(remotes, tunnel = TRUE, port = NULL) {
  port <- port %||% 40491
  
  # Create SSH config as quoted expression
  remote_config <- if (tunnel) {
    substitute(
      mirai::ssh_config(remotes, tunnel = TRUE),
      list(remotes = remotes)
    )
  } else {
    substitute(
      mirai::ssh_config(remotes),
      list(remotes = remotes)
    )
  }
  
  # URL configuration for tunneling
  url_config <- if (tunnel) {
    substitute(
      mirai::local_url(tcp = TRUE, port = port),
      list(port = port)
    )
  } else {
    NULL
  }
  
  dist_mirai(
    remote = remote_config,
    url = url_config,
    port = port,
    dispatcher = TRUE
  )
}

# Use the %||% operator from utils.R instead of redefining