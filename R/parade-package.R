#' @keywords internal
"_PACKAGE"

#' Declarative parallel dataflow with Future/Furrr and SLURM utilities
#'
#' The parade package provides a declarative, lazy, and compositional approach
#' to parallel dataflow in R. Built on top of the future/furrr ecosystem, it
#' offers typed schemas, artifact management, diagnostics, and HPC-friendly
#' distribution including seamless SLURM integration via future.batchtools.
#'
#' Key features include:
#' - Typed dataflow pipelines with dependency management
#' - Local and SLURM distributed execution
#' - Artifact persistence with automatic metadata
#' - Real-time job monitoring and dashboards
#' - Configurable resource management with profiles
#' - Comprehensive error handling and diagnostics
#'
#' @seealso
#' Core functions: [flow()], [stage()], [collect()], [submit()]
#' Distribution: [dist_local()], [dist_slurm()], [dist_mirai()], [dist_crew()], [distribute()]
#' Configuration: [parade_config_read()], [slurm_defaults_set()]
#' Monitoring: [script_top()], [jobs_top()], [script_metrics()]
#' @import tibble
#' @import tidyr
#' @import dplyr
#' @import rlang
#' @import vctrs
#' @import furrr
#' @import future
#' @import progressr
#' @import glue
#' @import jsonlite
#' @import digest
#' @importFrom purrr pmap compact map_chr map_dbl map_lgl map_int map_dfr
