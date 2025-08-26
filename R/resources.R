# Resources + templates ----------------------------------------------------
#' Create a list of SLURM resources with friendly parsing
#'
#' Builds a resource specification for SLURM job submission with convenient
#' parsing of time formats and memory specifications. Handles common
#' abbreviations and normalizes values for batchtools compatibility.
#'
#' @param partition SLURM partition name
#' @param time Time limit (accepts formats like '2h', '90min', 'H:MM:SS')
#' @param nodes Number of nodes required
#' @param ntasks Number of tasks
#' @param ntasks_per_node Number of tasks per node
#' @param cpus_per_task CPUs per task
#' @param ncpus Alias for cpus_per_task
#' @param mem Memory requirement (e.g., "4GB", "1000MB")
#' @param account SLURM account to charge
#' @param qos Quality of service level
#' @param modules Environment modules to load
#' @param omp_num_threads OpenMP thread count
#' @return Named list suitable for batchtools submitJobs resources
#' @export
#' @examples
#' batch_resources(time = "2h", mem = "4GB", cpus_per_task = 4)
#' batch_resources(partition = "gpu", time = "30min")
batch_resources <- function(partition = NULL,
                            time = NULL,                  # accepts '2h', '90min', '3600s', 'H:MM(:SS)'
                            nodes = NULL,
                            ntasks = NULL,
                            ntasks_per_node = NULL,
                            cpus_per_task = NULL,
                            ncpus = NULL,                 # alias -> cpus_per_task
                            mem = NULL,
                            account = NULL,
                            qos = NULL,
                            modules = NULL,
                            omp_num_threads = NULL) {
  # normalize time
  if (!is.null(time)) time <- .parade_norm_time(time)
  # alias
  if (!is.null(ncpus) && is.null(cpus_per_task)) cpus_per_task <- ncpus
  # pack
  compact <- function(x) x[!vapply(x, is.null, logical(1))]
  compact(list(partition=partition, time=time, nodes=nodes, ntasks=ntasks, ntasks_per_node=ntasks_per_node,
               cpus_per_task=cpus_per_task, mem=mem, account=account, qos=qos, modules=modules,
               omp_num_threads=omp_num_threads))
}

#' @keywords internal
.parade_norm_time <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x)) { secs <- as.integer(x); h <- secs %/% 3600; m <- (secs %% 3600) %/% 60; s <- secs %% 60; return(sprintf("%d:%02d:%02d", h, m, s)) }
  s <- tolower(gsub("\\s+", "", as.character(x)))
  if (grepl("^\\d+:\\d{2}(:\\d{2})?$", s)) { parts <- strsplit(s, ":")[[1]]; if (length(parts) == 2) return(sprintf("%d:%02d:00", as.integer(parts[0+1]), as.integer(parts[1+1]))); return(s) }
  m <- regexec("^(\\d+)(d|day|days|h|hr|hrs|hour|hours|m|min|mins|minute|minutes|s|sec|secs|second|seconds)$", s); g <- regmatches(s, m)[[1]]
  if (length(g) == 3) {
    n <- as.numeric(g[2]); u <- g[3]
    mult <- switch(u,
      "d"=86400, "day"=86400, "days"=86400,
      "h"=3600, "hr"=3600, "hrs"=3600, "hour"=3600, "hours"=3600,
      "m"=60, "min"=60, "mins"=60, "minute"=60, "minutes"=60,
      "s"=1, "sec"=1, "secs"=1, "second"=1, "seconds"=1, 1)
    secs <- as.integer(n * mult); h <- secs %/% 3600; m <- (secs %% 3600) %/% 60; s <- secs %% 60
    return(sprintf("%d:%02d:%02d", h, m, s))
  }
  stop("Cannot parse time value: ", x)
}
