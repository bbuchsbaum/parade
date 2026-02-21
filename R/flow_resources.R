# Stage-level resource resolution -------------------------------------------

#' @keywords internal
.parade_resource_hints <- function(resources = NULL,
                                   cpus = NULL,
                                   memory = NULL,
                                   time = NULL,
                                   where = "flow") {
  if (!is.character(where) || length(where) != 1L || !nzchar(where)) {
    where <- "flow"
  }

  if (!is.null(resources) && exists("resolve_resources", mode = "function")) {
    resolved <- try(resolve_resources(resources), silent = TRUE)
    if (!inherits(resolved, "try-error") && !is.null(resolved)) {
      resources <- resolved
    }
  }

  if (is.null(resources)) resources <- list()
  if (!is.list(resources)) {
    stop(sprintf("`%s` resources must be NULL or a named list.", where), call. = FALSE)
  }

  allowed <- c("cpus", "cpus_per_task", "ncpus", "memory", "mem", "time")
  unknown <- setdiff(names(resources), allowed)
  if (length(unknown)) {
    stop(
      sprintf(
        "`%s` resources only supports keys: %s (got: %s).",
        where,
        paste(allowed, collapse = ", "),
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cpus_aliases <- list(
    cpus = resources$cpus,
    cpus_per_task = resources$cpus_per_task,
    ncpus = resources$ncpus
  )
  cpus_present <- vapply(cpus_aliases, function(v) !is.null(v) && !.is_missing(v), logical(1))
  if (sum(cpus_present) > 1L) {
    cpus_vals <- unique(vapply(cpus_aliases[cpus_present], function(v) as.character(v)[1], character(1)))
    if (length(cpus_vals) > 1L) {
      stop(
        sprintf("`%s` resources has conflicting CPU hints (`cpus`, `cpus_per_task`, `ncpus`).", where),
        call. = FALSE
      )
    }
  }

  mem_present <- !is.null(resources$memory) && !.is_missing(resources$memory)
  mem_alias_present <- !is.null(resources$mem) && !.is_missing(resources$mem)
  if (mem_present && mem_alias_present) {
    if (!identical(as.character(resources$memory)[1], as.character(resources$mem)[1])) {
      stop(
        sprintf("`%s` resources has conflicting memory hints (`memory` and `mem`).", where),
        call. = FALSE
      )
    }
  }

  cpus0 <- resources$cpus %||% resources$cpus_per_task %||% resources$ncpus
  memory0 <- resources$memory %||% resources$mem
  time0 <- resources$time

  if (!is.null(cpus)) cpus0 <- cpus
  if (!is.null(memory)) memory0 <- memory
  if (!is.null(time)) time0 <- time

  if (!is.null(cpus0) && !.is_missing(cpus0)) {
    if (length(cpus0) != 1L) {
      stop(sprintf("`%s` cpus must be a length-1 value.", where), call. = FALSE)
    }
    cpus_num <- suppressWarnings(as.numeric(cpus0))
    if (is.na(cpus_num) || !is.finite(cpus_num) || cpus_num <= 0 || cpus_num != floor(cpus_num)) {
      stop(sprintf("`%s` cpus must be a positive integer.", where), call. = FALSE)
    }
    cpus0 <- as.integer(cpus_num)
  }

  if (!is.null(memory0) && !.is_missing(memory0)) {
    if (length(memory0) != 1L) {
      stop(sprintf("`%s` memory must be a length-1 value.", where), call. = FALSE)
    }
    mem_chr <- trimws(as.character(memory0))
    if (!nzchar(mem_chr)) {
      stop(sprintf("`%s` memory must not be empty.", where), call. = FALSE)
    }
    .validate_no_newlines(mem_chr, sprintf("%s memory", where))
    if (grepl("\\s", mem_chr)) {
      stop(sprintf("`%s` memory must not contain whitespace.", where), call. = FALSE)
    }

    mem_bytes <- NA_real_
    if (exists(".parade_parse_mem", mode = "function", inherits = TRUE)) {
      mem_bytes <- suppressWarnings(.parade_parse_mem(mem_chr))
    }
    if (is.na(mem_bytes)) {
      stop(
        sprintf("`%s` memory must be parseable (e.g. '8G', '16000M').", where),
        call. = FALSE
      )
    }
    memory0 <- mem_chr
  }

  if (!is.null(time0) && !.is_missing(time0)) {
    if (length(time0) != 1L) {
      stop(sprintf("`%s` time must be a length-1 value.", where), call. = FALSE)
    }
    time0 <- .parade_norm_time(time0)
  }

  out <- list()
  if (!is.null(cpus0)) out$cpus <- cpus0
  if (!is.null(memory0)) out$memory <- memory0
  if (!is.null(time0)) out$time <- time0
  out
}

#' @keywords internal
.parade_hints_to_slurm <- function(hints = NULL) {
  hints <- hints %||% list()
  out <- list()
  if ("cpus" %in% names(hints)) out$cpus_per_task <- hints$cpus
  if ("memory" %in% names(hints)) out$mem <- hints$memory
  if ("time" %in% names(hints)) out$time <- hints$time
  out
}

#' @keywords internal
.parade_slurm_to_hints <- function(resources = NULL) {
  resources <- resources %||% list()
  out <- list()
  if ("cpus_per_task" %in% names(resources)) {
    out$cpus <- resources$cpus_per_task
  } else if ("ncpus" %in% names(resources)) {
    out$cpus <- resources$ncpus
  } else if ("cpus" %in% names(resources)) {
    out$cpus <- resources$cpus
  }

  if ("mem" %in% names(resources)) {
    out$memory <- resources$mem
  } else if ("memory" %in% names(resources)) {
    out$memory <- resources$memory
  }

  if ("time" %in% names(resources)) out$time <- resources$time
  out
}

#' @keywords internal
.parade_normalize_slurm_resources <- function(resources = NULL, defaults = NULL) {
  merged <- utils::modifyList(defaults %||% list(), resources %||% list())

  if (!is.null(merged$memory)) {
    merged$mem <- merged$memory
    merged$memory <- NULL
  }
  if (!is.null(merged$cpus) && is.null(merged$cpus_per_task) && is.null(merged$ncpus)) {
    merged$cpus_per_task <- merged$cpus
    merged$cpus <- NULL
  }

  .validate_slurm_resource_values(merged)

  recognized <- c(
    "partition", "time", "nodes", "ntasks", "ntasks_per_node",
    "cpus_per_task", "ncpus", "mem", "account", "qos", "modules",
    "omp_num_threads"
  )
  batch_args <- merged[intersect(names(merged), recognized)]
  normalized <- do.call(batch_resources, batch_args)

  passthrough_names <- setdiff(names(merged), recognized)
  passthrough <- merged[paste0(passthrough_names)]
  utils::modifyList(normalized, passthrough)
}

#' @keywords internal
.parade_has_resource_hints <- function(fl) {
  flow_hints <- fl$options$resources %||% list()
  if (length(flow_hints)) return(TRUE)
  any(vapply(fl$stages %||% list(), function(st) length(st$resources %||% list()) > 0L, logical(1)))
}

#' @keywords internal
.parade_slurm_profile_name <- function(dist) {
  p <- dist$slurm$profile %||% "default"
  if (!is.character(p) || length(p) != 1L || !nzchar(p)) "default" else p
}

#' @keywords internal
.parade_slurm_base_resources <- function(fl, include_profile_defaults = FALSE) {
  dist <- fl$dist
  if (is.null(dist) || !identical(dist$backend, "slurm")) return(list())

  base <- dist$slurm$resources %||% list()
  if (!isTRUE(include_profile_defaults)) return(base)

  profile <- .parade_slurm_profile_name(dist)
  slurm_resources(resources = base, profile = profile)
}

#' @keywords internal
.parade_resolve_resource_key <- function(stage_hints, flow_hints, base_hints, key) {
  if (key %in% names(stage_hints)) return(list(value = stage_hints[[key]], source = "stage"))
  if (key %in% names(flow_hints)) return(list(value = flow_hints[[key]], source = "flow"))
  if (key %in% names(base_hints)) return(list(value = base_hints[[key]], source = "profile"))
  list(value = NULL, source = "none")
}

#' @keywords internal
.parade_stage_resource_table <- function(fl, include_profile_defaults = .parade_has_resource_hints(fl)) {
  stopifnot(inherits(fl, "parade_flow"))

  stages <- fl$stages %||% list()
  if (!length(stages)) {
    return(tibble::tibble(
      stage_id = character(),
      cpus = integer(),
      memory = character(),
      time = character(),
      cpus_source = character(),
      memory_source = character(),
      time_source = character(),
      slurm_resources = list()
    ))
  }

  base_slurm <- .parade_slurm_base_resources(fl, include_profile_defaults = include_profile_defaults)
  base_hints <- .parade_slurm_to_hints(base_slurm)
  flow_hints <- fl$options$resources %||% list()

  rows <- lapply(stages, function(st) {
    stage_hints <- st$resources %||% list()

    cpus_r <- .parade_resolve_resource_key(stage_hints, flow_hints, base_hints, "cpus")
    mem_r <- .parade_resolve_resource_key(stage_hints, flow_hints, base_hints, "memory")
    time_r <- .parade_resolve_resource_key(stage_hints, flow_hints, base_hints, "time")

    effective_hints <- list()
    if (!is.null(cpus_r$value)) effective_hints$cpus <- cpus_r$value
    if (!is.null(mem_r$value)) effective_hints$memory <- mem_r$value
    if (!is.null(time_r$value)) effective_hints$time <- time_r$value

    stage_slurm <- .parade_normalize_slurm_resources(
      resources = .parade_hints_to_slurm(effective_hints),
      defaults = base_slurm
    )

    tibble::tibble(
      stage_id = st$id,
      cpus = if ("cpus_per_task" %in% names(stage_slurm)) as.integer(stage_slurm$cpus_per_task) else NA_integer_,
      memory = if ("mem" %in% names(stage_slurm)) as.character(stage_slurm$mem) else NA_character_,
      time = if ("time" %in% names(stage_slurm)) as.character(stage_slurm$time) else NA_character_,
      cpus_source = cpus_r$source,
      memory_source = mem_r$source,
      time_source = time_r$source,
      slurm_resources = list(stage_slurm)
    )
  })

  tibble::as_tibble(vctrs::vec_rbind(!!!rows))
}

#' Inspect effective stage-level resource resolution
#'
#' Returns one row per stage with resolved resource hints and source layers.
#' Resolution follows:
#' 1. Stage hints (`stage(..., cpus/memory/time = ...)`)
#' 2. Flow defaults (`flow(..., cpus/memory/time = ...)`)
#' 3. SLURM profile/site defaults when applicable
#'
#' @param fl A [flow()] object.
#' @return Tibble with resolved per-stage resource values and source columns.
#' @export
flow_stage_resources <- function(fl) {
  stopifnot(inherits(fl, "parade_flow"))
  .parade_stage_resource_table(fl, include_profile_defaults = .parade_has_resource_hints(fl))
}

#' @keywords internal
.parade_pick_max_time <- function(values) {
  if (!length(values)) return(NULL)
  secs <- vapply(values, function(v) {
    parsed <- .parade_parse_hms(.parade_norm_time(v))
    if (is.na(parsed) || !is.finite(parsed)) {
      stop("Unable to parse resolved time resources for stage envelope.", call. = FALSE)
    }
    as.numeric(parsed)
  }, numeric(1))
  .parade_norm_time(max(secs))
}

#' @keywords internal
.parade_pick_max_memory <- function(values) {
  if (!length(values)) return(NULL)
  bytes <- vapply(values, function(v) {
    parsed <- suppressWarnings(.parade_parse_mem(as.character(v)))
    if (is.na(parsed) || !is.finite(parsed)) {
      stop("Unable to parse resolved memory resources for stage envelope.", call. = FALSE)
    }
    as.numeric(parsed)
  }, numeric(1))
  values[[which.max(bytes)]]
}

#' @keywords internal
.parade_pick_max_cpus <- function(values) {
  if (!length(values)) return(NULL)
  cpus <- vapply(values, function(v) suppressWarnings(as.integer(v)), integer(1))
  if (any(is.na(cpus)) || any(cpus <= 0L)) {
    stop("Resolved CPU resources must be positive integers.", call. = FALSE)
  }
  as.integer(max(cpus))
}

#' @keywords internal
.parade_slurm_resource_envelope <- function(stage_tbl, base_slurm = list()) {
  if (!nrow(stage_tbl)) {
    return(.parade_normalize_slurm_resources(base_slurm, defaults = list()))
  }

  collect_vals <- function(key) {
    vals <- lapply(stage_tbl$slurm_resources, function(res) {
      if (key %in% names(res)) res[[key]] else NULL
    })
    vals[!vapply(vals, is.null, logical(1))]
  }

  cpus_vals <- collect_vals("cpus_per_task")
  mem_vals <- collect_vals("mem")
  time_vals <- collect_vals("time")

  out <- base_slurm
  if (length(cpus_vals)) {
    out$cpus_per_task <- .parade_pick_max_cpus(cpus_vals)
  } else {
    out$cpus_per_task <- NULL
  }
  if (length(mem_vals)) {
    out$mem <- .parade_pick_max_memory(mem_vals)
  } else {
    out$mem <- NULL
  }
  if (length(time_vals)) {
    out$time <- .parade_pick_max_time(time_vals)
  } else {
    out$time <- NULL
  }

  .parade_normalize_slurm_resources(out, defaults = list())
}

#' @keywords internal
.parade_submit_slurm_resources <- function(fl, dist) {
  if (is.null(dist) || !identical(dist$backend, "slurm")) return(list())

  if (!.parade_has_resource_hints(fl)) {
    return(dist$slurm$resources %||% list())
  }

  base_slurm <- .parade_slurm_base_resources(fl, include_profile_defaults = TRUE)
  stage_tbl <- .parade_stage_resource_table(fl, include_profile_defaults = TRUE)

  if (!nrow(stage_tbl)) {
    flow_hints <- fl$options$resources %||% list()
    return(.parade_normalize_slurm_resources(
      resources = .parade_hints_to_slurm(flow_hints),
      defaults = base_slurm
    ))
  }

  .parade_slurm_resource_envelope(stage_tbl, base_slurm = base_slurm)
}
