# Flow planning and fingerprints -------------------------------------------

#' @keywords internal
.parade_stage_io_spec <- function(st) {
  io <- st$io %||% list()
  list(
    inputs = io$inputs %||% NULL,
    input_artifacts = io$input_artifacts %||% character(),
    outputs = io$outputs %||% names(st$ptype),
    mode = io$mode %||% "off"
  )
}

#' @keywords internal
.parade_stage_required_inputs <- function(st, io = .parade_stage_io_spec(st)) {
  if (!is.null(io$inputs)) return(io$inputs)
  inferred <- names(formals(st$f))
  inferred <- inferred %||% character()
  setdiff(inferred, "...")
}

#' @keywords internal
.parade_validate_stage_inputs_runtime <- function(st, args) {
  io <- .parade_stage_io_spec(st)
  mode <- io$mode %||% "off"
  if (identical(mode, "off")) return(list(ok = TRUE, mode = mode))

  required <- .parade_stage_required_inputs(st, io = io)
  missing_inputs <- required[!required %in% names(args)]
  if (length(missing_inputs)) {
    return(list(
      ok = FALSE,
      mode = mode,
      reason_code = "missing_input",
      message = sprintf(
        "Stage '%s' missing required inputs: %s",
        st$id, paste(missing_inputs, collapse = ", ")
      ),
      missing_inputs = missing_inputs,
      missing_artifacts = character()
    ))
  }

  art_required <- io$input_artifacts %||% character()
  not_artifact <- character()
  if (length(art_required)) {
    for (nm in art_required) {
      v <- args[[nm]]
      is_ref <- exists(".is_file_ref", mode = "function", inherits = TRUE) && isTRUE(.is_file_ref(v))
      if (!is_ref) not_artifact <- c(not_artifact, nm)
    }
  }
  if (length(not_artifact)) {
    return(list(
      ok = FALSE,
      mode = mode,
      reason_code = "not_artifact_input",
      message = sprintf(
        "Stage '%s' expected artifact inputs for: %s",
        st$id, paste(not_artifact, collapse = ", ")
      ),
      missing_inputs = character(),
      missing_artifacts = not_artifact
    ))
  }

  list(ok = TRUE, mode = mode)
}

#' @keywords internal
.parade_validate_stage_outputs_runtime <- function(st, result) {
  io <- .parade_stage_io_spec(st)
  mode <- io$mode %||% "off"
  if (identical(mode, "off")) return(list(ok = TRUE, mode = mode))

  expected <- io$outputs %||% character()
  if (!length(expected)) return(list(ok = TRUE, mode = mode))

  result_names <- if (is.list(result)) names(result) %||% character() else character()
  missing_outputs <- expected[!expected %in% result_names]
  if (length(missing_outputs)) {
    return(list(
      ok = FALSE,
      mode = mode,
      reason_code = "missing_output",
      message = sprintf(
        "Stage '%s' missing declared outputs: %s",
        st$id, paste(missing_outputs, collapse = ", ")
      )
    ))
  }

  list(ok = TRUE, mode = mode)
}

#' @keywords internal
.parade_stage_artifact_paths <- function(st, row) {
  if (is.null(st$sink) || !length(st$sink$fields %||% character())) return(character())
  vapply(st$sink$fields, function(field) .build_path(st$sink, row, st$id, field), character(1))
}

#' @keywords internal
.parade_plan_stage_decision <- function(st,
                                        row,
                                        available_fields,
                                        available_artifacts,
                                        completed_stage_ids) {
  io <- .parade_stage_io_spec(st)
  io_mode <- io$mode %||% "off"
  required <- .parade_stage_required_inputs(st, io = io)
  missing_inputs <- required[!required %in% available_fields]
  required_artifacts <- io$input_artifacts %||% character()
  missing_artifacts <- required_artifacts[!required_artifacts %in% available_artifacts]
  missing_deps <- setdiff(st$needs %||% character(), completed_stage_ids)
  artifact_paths <- .parade_stage_artifact_paths(st, row)

  if (length(missing_deps)) {
    return(list(
      action = "blocked",
      reason_code = "missing_dependency",
      io_mode = io_mode,
      missing_inputs = character(),
      missing_artifacts = character(),
      artifact_paths = artifact_paths
    ))
  }

  if (identical(io_mode, "error") && length(missing_inputs)) {
    return(list(
      action = "blocked",
      reason_code = "missing_input",
      io_mode = io_mode,
      missing_inputs = missing_inputs,
      missing_artifacts = character(),
      artifact_paths = artifact_paths
    ))
  }

  if (identical(io_mode, "error") && length(missing_artifacts)) {
    return(list(
      action = "blocked",
      reason_code = "missing_artifact",
      io_mode = io_mode,
      missing_inputs = missing_inputs,
      missing_artifacts = missing_artifacts,
      artifact_paths = artifact_paths
    ))
  }

  if (identical(io_mode, "warn") && length(missing_inputs)) {
    return(list(
      action = "execute",
      reason_code = "warning_missing_input",
      io_mode = io_mode,
      missing_inputs = missing_inputs,
      missing_artifacts = character(),
      artifact_paths = artifact_paths
    ))
  }

  if (identical(io_mode, "warn") && length(missing_artifacts)) {
    return(list(
      action = "execute",
      reason_code = "warning_missing_artifact",
      io_mode = io_mode,
      missing_inputs = missing_inputs,
      missing_artifacts = missing_artifacts,
      artifact_paths = artifact_paths
    ))
  }

  if (is.null(st$sink)) {
    return(list(
      action = "execute",
      reason_code = "no_sink",
      io_mode = io_mode,
      missing_inputs = missing_inputs,
      missing_artifacts = missing_artifacts,
      artifact_paths = character()
    ))
  }

  overwrite <- st$sink$overwrite %||% "skip"
  exists_vec <- if (length(artifact_paths)) file.exists(artifact_paths) else logical()

  if (identical(overwrite, "error") && any(exists_vec)) {
    return(list(
      action = "blocked",
      reason_code = "artifact_conflict",
      io_mode = io_mode,
      missing_inputs = missing_inputs,
      missing_artifacts = missing_artifacts,
      artifact_paths = artifact_paths
    ))
  }

  if (identical(overwrite, "skip") && length(artifact_paths) && all(exists_vec)) {
    return(list(
      action = "reuse",
      reason_code = "artifact_exists",
      io_mode = io_mode,
      missing_inputs = missing_inputs,
      missing_artifacts = missing_artifacts,
      artifact_paths = artifact_paths
    ))
  }

  if (!identical(overwrite, "skip")) {
    return(list(
      action = "execute",
      reason_code = "overwrite_policy",
      io_mode = io_mode,
      missing_inputs = missing_inputs,
      missing_artifacts = missing_artifacts,
      artifact_paths = artifact_paths
    ))
  }

  list(
    action = "execute",
    reason_code = "artifact_missing",
    io_mode = io_mode,
    missing_inputs = missing_inputs,
    missing_artifacts = missing_artifacts,
    artifact_paths = artifact_paths
  )
}

#' Compute a deterministic fingerprint for a flow
#'
#' Fingerprints include stage definitions and the selected parameter grid.
#' The hash changes when relevant stage code/specification or params change.
#'
#' @param fl A [flow()] object.
#' @param limit Optional row limit for the grid before hashing.
#' @param engine Engine tag included in the fingerprint (default `"sequential"`).
#' @param validate Validation mode tag included in the fingerprint.
#' @return A SHA1 fingerprint string.
#' @export
flow_fingerprint <- function(fl, limit = NULL, engine = "sequential", validate = "light") {
  stopifnot(inherits(fl, "parade_flow"))
  grid <- fl$grid
  if (!is.null(limit)) grid <- utils::head(grid, limit)
  .parade_flow_run_key(fl, grid = grid, engine = engine, validate = validate)
}

#' Build a deterministic execution plan with reason codes
#'
#' Returns one row per `(grid_row, stage)` with planned action:
#' - `execute`
#' - `reuse` (artifact already exists and sink overwrite policy is `skip`)
#' - `blocked`
#'
#' @param fl A [flow()] object.
#' @param limit Optional limit on number of grid rows to plan.
#' @return A tibble plan with reason codes and stage fingerprints.
#' @export
flow_plan <- function(fl, limit = NULL) {
  stopifnot(inherits(fl, "parade_flow"))
  grid <- fl$grid
  if (!is.null(limit)) grid <- utils::head(grid, limit)
  order <- .toposort(fl$stages)
  stage_resources <- .parade_stage_resource_table(fl, include_profile_defaults = .parade_has_resource_hints(fl))
  stage_resources_map <- split(stage_resources, stage_resources$stage_id)
  if (length(order) == 0L || nrow(grid) == 0L) {
    return(tibble::tibble(
      run_key = character(),
      row_index = integer(),
      row_id = character(),
      stage_index = integer(),
      stage_id = character(),
      io_mode = character(),
      action = character(),
      reason_code = character(),
      stage_fingerprint = character(),
      missing_inputs = list(),
      missing_artifacts = list(),
      declared_inputs = list(),
      declared_outputs = list(),
      artifact_paths = list(),
      resource_cpus = integer(),
      resource_memory = character(),
      resource_time = character(),
      resource_cpus_source = character(),
      resource_memory_source = character(),
      resource_time_source = character(),
      resource_slurm = list()
    ))
  }
  run_key <- flow_fingerprint(fl, limit = limit)
  stage_ids <- vapply(fl$stages, function(s) s$id, character(1))
  stage_map <- setNames(fl$stages, stage_ids)
  stage_index <- seq_along(order)
  names(stage_index) <- order

  rows <- vector("list", nrow(grid) * length(order))
  k <- 0L
  for (i in seq_len(nrow(grid))) {
    row <- as.list(grid[i, , drop = FALSE])
    row_id <- digest::digest(row, algo = "sha1")
    available_fields <- names(row)
    available_artifacts <- character()
    completed_stage_ids <- character()
    fp_map <- list()
    for (sid in order) {
      st <- stage_map[[sid]]
      io <- .parade_stage_io_spec(st)
      required_inputs <- .parade_stage_required_inputs(st, io = io)
      declared_outputs <- io$outputs %||% names(st$ptype)
      decision <- .parade_plan_stage_decision(
        st = st,
        row = row,
        available_fields = available_fields,
        available_artifacts = available_artifacts,
        completed_stage_ids = completed_stage_ids
      )
      upstream_fp <- unname(unlist(fp_map[st$needs %||% character()], use.names = FALSE))
      stage_fp <- digest::digest(list(
        run_key = run_key,
        row_id = row_id,
        stage_id = sid,
        stage_sig = .parade_stage_signature(st),
        upstream_fp = upstream_fp
      ), algo = "sha1")
      fp_map[[sid]] <- stage_fp
      if (!identical(decision$action, "blocked")) {
        completed_stage_ids <- c(completed_stage_ids, sid)
        available_fields <- union(available_fields, declared_outputs)
        if (isTRUE(st$prefix)) {
          available_fields <- union(available_fields, paste0(sid, ".", declared_outputs))
        }
        if (!is.null(st$sink) && length(st$sink$fields %||% character())) {
          available_artifacts <- union(available_artifacts, st$sink$fields)
          if (isTRUE(st$prefix)) {
            available_artifacts <- union(available_artifacts, paste0(sid, ".", st$sink$fields))
          }
        }
      }
      k <- k + 1L
      rs <- stage_resources_map[[sid]]
      if (is.null(rs) || nrow(rs) == 0L) {
        rs <- tibble::tibble(
          cpus = NA_integer_,
          memory = NA_character_,
          time = NA_character_,
          cpus_source = "none",
          memory_source = "none",
          time_source = "none",
          slurm_resources = list(list())
        )
      }
      rows[[k]] <- tibble::tibble(
        run_key = run_key,
        row_index = as.integer(i),
        row_id = row_id,
        stage_index = as.integer(stage_index[[sid]]),
        stage_id = sid,
        io_mode = as.character(decision$io_mode),
        action = as.character(decision$action),
        reason_code = as.character(decision$reason_code),
        stage_fingerprint = stage_fp,
        missing_inputs = list(decision$missing_inputs %||% character()),
        missing_artifacts = list(decision$missing_artifacts %||% character()),
        declared_inputs = list(required_inputs %||% character()),
        declared_outputs = list(declared_outputs %||% character()),
        artifact_paths = list(decision$artifact_paths %||% character()),
        resource_cpus = as.integer(rs$cpus[[1]] %||% NA_integer_),
        resource_memory = as.character(rs$memory[[1]] %||% NA_character_),
        resource_time = as.character(rs$time[[1]] %||% NA_character_),
        resource_cpus_source = as.character(rs$cpus_source[[1]] %||% "none"),
        resource_memory_source = as.character(rs$memory_source[[1]] %||% "none"),
        resource_time_source = as.character(rs$time_source[[1]] %||% "none"),
        resource_slurm = list(rs$slurm_resources[[1]] %||% list())
      )
    }
  }
  out <- tibble::as_tibble(vctrs::vec_rbind(!!!rows[seq_len(k)]))
  ord <- order(out$row_index, out$stage_index, out$stage_id)
  out[ord, , drop = FALSE]
}
