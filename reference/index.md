# Package index

## All functions

- [`account()`](https://bbuchsbaum.github.io/parade/reference/account.md)
  : Set account for a resource profile

- [`args()`](https://bbuchsbaum.github.io/parade/reference/args.md) :
  Auto-detect argument type

- [`args_call()`](https://bbuchsbaum.github.io/parade/reference/args_call.md)
  : Create argument list for function calls

- [`args_cli()`](https://bbuchsbaum.github.io/parade/reference/args_cli.md)
  : Create argument list for CLI scripts

- [`artifact()`](https://bbuchsbaum.github.io/parade/reference/artifact.md)
  : Alias for file_ref function

- [`artifact_catalog()`](https://bbuchsbaum.github.io/parade/reference/artifact_catalog.md)
  : Artifact catalog utilities

- [`artifact_catalog_search()`](https://bbuchsbaum.github.io/parade/reference/artifact_catalog_search.md)
  : Search an artifact catalog

- [`as.list(`*`<parade_profile>`*`)`](https://bbuchsbaum.github.io/parade/reference/as.list.parade_profile.md)
  : Convert profile to list for slurm_resources()

- [`as_jobset()`](https://bbuchsbaum.github.io/parade/reference/as_jobset.md)
  : Convert a job or list of jobs to a jobset

- [`as_tibble(`*`<parade_jobset>`*`)`](https://bbuchsbaum.github.io/parade/reference/as_tibble.parade_jobset.md)
  : Convert jobset to tibble

- [`await()`](https://bbuchsbaum.github.io/parade/reference/await.md) :
  Wait for jobs to complete

- [`balance_by()`](https://bbuchsbaum.github.io/parade/reference/balance_by.md)
  : Balance work across groups

- [`batch_resources()`](https://bbuchsbaum.github.io/parade/reference/batch_resources.md)
  : Create a list of SLURM resources with friendly parsing

- [`blob()`](https://bbuchsbaum.github.io/parade/reference/blob.md) :
  Create a blob type specification

- [`c(`*`<parade_jobset>`*`)`](https://bbuchsbaum.github.io/parade/reference/c.parade_jobset.md)
  : Combine jobsets

- [`cancel()`](https://bbuchsbaum.github.io/parade/reference/cancel.md)
  : Cancel all jobs in a jobset

- [`chr()`](https://bbuchsbaum.github.io/parade/reference/chr.md) :
  Create a character type specification

- [`chunk_by()`](https://bbuchsbaum.github.io/parade/reference/chunk_by.md)
  : Chunk data into groups

- [`collect()`](https://bbuchsbaum.github.io/parade/reference/collect.md)
  : Collect results from all jobs in a jobset

- [`collect(`*`<parade_flow>`*`)`](https://bbuchsbaum.github.io/parade/reference/collect.parade_flow.md)
  : Execute a parade flow and collect results

- [`collect_result()`](https://bbuchsbaum.github.io/parade/reference/collect_result.md)
  : Collect results from a job

- [`combine_grids()`](https://bbuchsbaum.github.io/parade/reference/combine_grids.md)
  : Combine multiple grids

- [`completed()`](https://bbuchsbaum.github.io/parade/reference/completed.md)
  : Select completed jobs

- [`contract()`](https://bbuchsbaum.github.io/parade/reference/contract.md)
  : Define a validation contract for stage outputs

- [`cpus()`](https://bbuchsbaum.github.io/parade/reference/cpus.md) :
  Set CPU count for a resource profile

- [`ctr_field()`](https://bbuchsbaum.github.io/parade/reference/ctr_field.md)
  : Define a contract field specification

- [`dbl()`](https://bbuchsbaum.github.io/parade/reference/dbl.md) :
  Create a double/numeric type specification

- [`deferred_await()`](https://bbuchsbaum.github.io/parade/reference/deferred_await.md)
  : Wait for deferred execution to complete

- [`deferred_cancel()`](https://bbuchsbaum.github.io/parade/reference/deferred_cancel.md)
  : Cancel deferred execution jobs

- [`deferred_collect()`](https://bbuchsbaum.github.io/parade/reference/deferred_collect.md)
  : Collect results from deferred execution

- [`deferred_status()`](https://bbuchsbaum.github.io/parade/reference/deferred_status.md)
  : Get status of a deferred execution

- [`diagnostics()`](https://bbuchsbaum.github.io/parade/reference/diagnostics.md)
  : Extract diagnostic information from flow results

- [`digest()`](https://bbuchsbaum.github.io/parade/reference/digest.md)
  : Generate job names from content digest

- [`dist_local()`](https://bbuchsbaum.github.io/parade/reference/dist_local.md)
  : Create local distribution specification

- [`dist_mirai()`](https://bbuchsbaum.github.io/parade/reference/dist_mirai.md)
  : Create mirai distribution specification

- [`dist_slurm()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm.md)
  : Create SLURM distribution specification

- [`dist_slurm_profile()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm_profile.md)
  : Convenience: SLURM distribution from a named profile

- [`distribute()`](https://bbuchsbaum.github.io/parade/reference/distribute.md)
  : Add distribution settings to a parade flow

- [`dry_run()`](https://bbuchsbaum.github.io/parade/reference/dry_run.md)
  : Dry run job submission

- [`dry_run(`*`<default>`*`)`](https://bbuchsbaum.github.io/parade/reference/dry_run.default.md)
  : Dry run for job submission (default)

- [`dry_run(`*`<parade_flow>`*`)`](https://bbuchsbaum.github.io/parade/reference/dry_run.parade_flow.md)
  : Dry-run a flow: show plan and counts without executing

- [`expand_path_macros_enhanced()`](https://bbuchsbaum.github.io/parade/reference/expand_path_macros_enhanced.md)
  : Expand path macros with enhanced patterns

- [`explain()`](https://bbuchsbaum.github.io/parade/reference/explain.md)
  : Explain what will be executed

- [`explain(`*`<default>`*`)`](https://bbuchsbaum.github.io/parade/reference/explain.default.md)
  : Explain a job submission (default)

- [`explain(`*`<parade_flow>`*`)`](https://bbuchsbaum.github.io/parade/reference/explain.parade_flow.md)
  : Explain a flow: DAG + distribution + sinks

- [`failed()`](https://bbuchsbaum.github.io/parade/reference/failed.md)
  : Select failed jobs

- [`failed(`*`<data.frame>`*`)`](https://bbuchsbaum.github.io/parade/reference/failed.data.frame.md)
  : Extract failed rows from flow results

- [`file_ref()`](https://bbuchsbaum.github.io/parade/reference/file_ref.md)
  : Create a file reference type specification

- [`flow()`](https://bbuchsbaum.github.io/parade/reference/flow.md) :
  Create a parade flow for declarative data processing

- [`flow_control()`](https://bbuchsbaum.github.io/parade/reference/flow_control.md)
  : Combine flow control policies

- [`get_errors()`](https://bbuchsbaum.github.io/parade/reference/get_errors.md)
  : Get collected errors from a policy

- [`get_sink_format()`](https://bbuchsbaum.github.io/parade/reference/get_sink_format.md)
  : Get a registered sink format

- [`glob()`](https://bbuchsbaum.github.io/parade/reference/glob.md) :
  Glob file patterns

- [`glue_name()`](https://bbuchsbaum.github.io/parade/reference/glue_name.md)
  : Generate job names using glue-style templates

- [`gpus()`](https://bbuchsbaum.github.io/parade/reference/gpus.md) :
  Set GPU count for a resource profile

- [`grid()`](https://bbuchsbaum.github.io/parade/reference/grid.md) :
  Create parameter grid for job submission

- [`guard_packages()`](https://bbuchsbaum.github.io/parade/reference/guard_packages.md)
  : Guard that required packages are available

- [`has_sink_format()`](https://bbuchsbaum.github.io/parade/reference/has_sink_format.md)
  : Check if a sink format is registered

- [`in_waves_of()`](https://bbuchsbaum.github.io/parade/reference/in_waves_of.md)
  : Submit jobs in waves with controlled parallelism

- [`index()`](https://bbuchsbaum.github.io/parade/reference/index-topic.md)
  : Generate job names from indices

- [`int()`](https://bbuchsbaum.github.io/parade/reference/int.md) :
  Create an integer type specification

- [`is_done()`](https://bbuchsbaum.github.io/parade/reference/is_done.md)
  : Check if a job is done

- [`is_flow_control()`](https://bbuchsbaum.github.io/parade/reference/is_flow_control.md)
  : Check if object is a flow control policy

- [`isa()`](https://bbuchsbaum.github.io/parade/reference/isa.md) :
  Create a class-based type specification

- [`job_status()`](https://bbuchsbaum.github.io/parade/reference/job_status.md)
  : Get job status

- [`jobs_top()`](https://bbuchsbaum.github.io/parade/reference/jobs_top.md)
  : Live dashboard for multiple SLURM jobs

- [`length(`*`<parade_jobset>`*`)`](https://bbuchsbaum.github.io/parade/reference/length.parade_jobset.md)
  : Get length of jobset

- [`lgl()`](https://bbuchsbaum.github.io/parade/reference/lgl.md) :
  Create a logical type specification

- [`lhs_grid()`](https://bbuchsbaum.github.io/parade/reference/lhs_grid.md)
  : Create a Latin hypercube sample for parameter exploration

- [`list_sink_formats()`](https://bbuchsbaum.github.io/parade/reference/list_sink_formats.md)
  : List registered sink formats

- [`list_submit_backends()`](https://bbuchsbaum.github.io/parade/reference/list_submit_backends.md)
  : List available submit backends

- [`lst()`](https://bbuchsbaum.github.io/parade/reference/lst.md) :
  Create a list type specification

- [`manifest()`](https://bbuchsbaum.github.io/parade/reference/manifest.md)
  : Create artifact manifest from sidecar files

- [`max_in_flight()`](https://bbuchsbaum.github.io/parade/reference/max_in_flight.md)
  : Limit maximum concurrent jobs

- [`maybe()`](https://bbuchsbaum.github.io/parade/reference/maybe.md) :
  Create an optional type specification

- [`maybe_neurovol()`](https://bbuchsbaum.github.io/parade/reference/maybe_neurovol.md)
  : Optional neuroimaging volume

- [`mem()`](https://bbuchsbaum.github.io/parade/reference/mem.md) : Set
  memory limit for a resource profile

- [`mirai_available()`](https://bbuchsbaum.github.io/parade/reference/mirai_available.md)
  : Check mirai availability

- [`mirai_dispatcher_status()`](https://bbuchsbaum.github.io/parade/reference/mirai_dispatcher_status.md)
  : Get mirai dispatcher status

- [`mirai_init()`](https://bbuchsbaum.github.io/parade/reference/mirai_init.md)
  : Initialize mirai for parade

- [`mirai_scale()`](https://bbuchsbaum.github.io/parade/reference/mirai_scale.md)
  : Scale mirai daemons

- [`mirai_status()`](https://bbuchsbaum.github.io/parade/reference/mirai_status.md)
  : Get mirai daemon status

- [`mirai_stop()`](https://bbuchsbaum.github.io/parade/reference/mirai_stop.md)
  : Stop all mirai daemons

- [`name_by()`](https://bbuchsbaum.github.io/parade/reference/name_by.md)
  : Create a custom naming function

- [`name_digest()`](https://bbuchsbaum.github.io/parade/reference/name_digest.md)
  :

  Alias for digest-based naming that avoids masking
  [`digest::digest`](https://eddelbuettel.github.io/digest/man/digest.html)

- [`neurovol()`](https://bbuchsbaum.github.io/parade/reference/neurovol.md)
  : Neuroimaging volume type specification

- [`` `%||%` ``](https://bbuchsbaum.github.io/parade/reference/null-coalesce.md)
  : Null-coalescing operator

- [`omit()`](https://bbuchsbaum.github.io/parade/reference/omit.md) :
  Create an omit sentinel for resource removal

- [`on_error()`](https://bbuchsbaum.github.io/parade/reference/on_error.md)
  : Define error handling policy for jobs

- [`on_error_retry()`](https://bbuchsbaum.github.io/parade/reference/on_error_retry.md)
  : Shorthand for retry error policy

- [`one_of()`](https://bbuchsbaum.github.io/parade/reference/one_of.md)
  : Create a union type specification

- [`open_logs()`](https://bbuchsbaum.github.io/parade/reference/open_logs.md)
  : Open log files for a job

- [`open_logs(`*`<default>`*`)`](https://bbuchsbaum.github.io/parade/reference/open_logs.default.md)
  : Fallback log opener for list-like job objects

- [`open_logs(`*`<parade_jobset>`*`)`](https://bbuchsbaum.github.io/parade/reference/open_logs.parade_jobset.md)
  : Open log files for a jobset

- [`pack()`](https://bbuchsbaum.github.io/parade/reference/pack.md) :
  Pack a schema into a structured type

- [`parade_config_path()`](https://bbuchsbaum.github.io/parade/reference/parade_config_path.md)
  : Locate the parade configuration file

- [`parade_config_read()`](https://bbuchsbaum.github.io/parade/reference/parade_config_read.md)
  : Read parade configuration

- [`parade_config_write()`](https://bbuchsbaum.github.io/parade/reference/parade_config_write.md)
  : Write parade configuration

- [`parade_dashboard()`](https://bbuchsbaum.github.io/parade/reference/parade_dashboard.md)
  : Unified dashboard for parade jobs

- [`parade_doctor()`](https://bbuchsbaum.github.io/parade/reference/parade_doctor.md)
  : Quick setup checks for parade

- [`parade_init_hpc()`](https://bbuchsbaum.github.io/parade/reference/parade_init_hpc.md)
  : Initialize parade for HPC use

- [`parade_options()`](https://bbuchsbaum.github.io/parade/reference/parade_options.md)
  : Global parade options (get/set)

- [`param_grid()`](https://bbuchsbaum.github.io/parade/reference/param_grid.md)
  : Create a parameter grid for flows

- [`partition()`](https://bbuchsbaum.github.io/parade/reference/partition.md)
  : Set partition for a resource profile

- [`path`](https://bbuchsbaum.github.io/parade/reference/path.md) : Path
  object with convenient accessors

- [`path_here()`](https://bbuchsbaum.github.io/parade/reference/path_here.md)
  : Resolve a path using configured aliases

- [`path_patterns`](https://bbuchsbaum.github.io/parade/reference/path_patterns.md)
  : Common path patterns

- [`path_template()`](https://bbuchsbaum.github.io/parade/reference/path_template.md)
  : Create a path template builder

- [`paths_export()`](https://bbuchsbaum.github.io/parade/reference/paths_export.md)
  : Generate shell exports for the current path configuration

- [`paths_get()`](https://bbuchsbaum.github.io/parade/reference/paths_get.md)
  : Get current parade path configuration

- [`paths_init()`](https://bbuchsbaum.github.io/parade/reference/paths_init.md)
  : Initialize parade path configuration

- [`paths_set()`](https://bbuchsbaum.github.io/parade/reference/paths_set.md)
  : Set specific parade paths

- [`paths_validate()`](https://bbuchsbaum.github.io/parade/reference/paths_validate.md)
  : Validate parade path configuration

- [`pending()`](https://bbuchsbaum.github.io/parade/reference/pending.md)
  : Select pending jobs

- [`pipeline()`](https://bbuchsbaum.github.io/parade/reference/pipeline.md)
  : Create a parade pipeline (alias for flow)

- [`pred()`](https://bbuchsbaum.github.io/parade/reference/pred.md) :
  Create a predicate-based type specification

- [`preflight()`](https://bbuchsbaum.github.io/parade/reference/preflight.md)
  : Preflight checks for a flow

- [`print(`*`<parade_error_policy>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_error_policy.md)
  : Print method for error policies

- [`print(`*`<parade_flow>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_flow.md)
  : Print method for parade flows

- [`print(`*`<parade_flow_control>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_flow_control.md)
  : Print method for flow control policies

- [`print(`*`<parade_job>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_job.md)
  : Print method for parade job objects

- [`print(`*`<parade_jobset>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_jobset.md)
  : Print method for parade_jobset

- [`print(`*`<parade_local_job>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_local_job.md)
  : Print method for local jobs

- [`print(`*`<parade_path>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_path.md)
  : Print method for path object

- [`print(`*`<parade_profile>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_profile.md)
  : Print method for resource profiles

- [`print(`*`<parade_script_job>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.parade_script_job.md)
  : Print method for parade script jobs

- [`print(`*`<param_grid>`*`)`](https://bbuchsbaum.github.io/parade/reference/print.param_grid.md)
  : Print method for parameter grids

- [`profile()`](https://bbuchsbaum.github.io/parade/reference/profile.md)
  : Create a resource profile for SLURM jobs

- [`profile_clear()`](https://bbuchsbaum.github.io/parade/reference/profile_clear.md)
  : Clear all registered profiles

- [`profile_get()`](https://bbuchsbaum.github.io/parade/reference/profile_get.md)
  : Get a registered resource profile

- [`profile_init_defaults()`](https://bbuchsbaum.github.io/parade/reference/profile_init_defaults.md)
  : Initialize default resource profiles

- [`profile_list()`](https://bbuchsbaum.github.io/parade/reference/profile_list.md)
  : List all registered resource profiles

- [`profile_register()`](https://bbuchsbaum.github.io/parade/reference/profile_register.md)
  : Register a named resource profile

- [`profile_remove()`](https://bbuchsbaum.github.io/parade/reference/profile_remove.md)
  : Remove a registered resource profile

- [`progress()`](https://bbuchsbaum.github.io/parade/reference/progress.md)
  : Show progress for jobset completion

- [`register_sink_format()`](https://bbuchsbaum.github.io/parade/reference/register_sink_format.md)
  : Register a sink format

- [`register_submit_backend()`](https://bbuchsbaum.github.io/parade/reference/register_submit_backend.md)
  : Register a submit backend

- [`registry_clean()`](https://bbuchsbaum.github.io/parade/reference/registry_clean.md)
  : Clean up old jobs from registry

- [`registry_ls()`](https://bbuchsbaum.github.io/parade/reference/registry_ls.md)
  : List jobs in registry

- [`res_account()`](https://bbuchsbaum.github.io/parade/reference/res_account.md)
  : Alias for account()

- [`res_cpus()`](https://bbuchsbaum.github.io/parade/reference/res_cpus.md)
  : Alias for cpus()

- [`res_mem()`](https://bbuchsbaum.github.io/parade/reference/res_mem.md)
  : Alias for mem()

- [`res_partition()`](https://bbuchsbaum.github.io/parade/reference/res_partition.md)
  : Alias for partition()

- [`res_time()`](https://bbuchsbaum.github.io/parade/reference/res_time.md)
  : Set time limit for a resource profile (non-masking)

- [`resolve_path()`](https://bbuchsbaum.github.io/parade/reference/resolve_path.md)
  : Resolve paths with URI-style aliases

- [`retry()`](https://bbuchsbaum.github.io/parade/reference/retry.md) :
  Retry failed jobs in a jobset

- [`returns()`](https://bbuchsbaum.github.io/parade/reference/returns.md)
  : Define expected return schema for a stage function

- [`running()`](https://bbuchsbaum.github.io/parade/reference/running.md)
  : Select running jobs

- [`scaffold_batch_template()`](https://bbuchsbaum.github.io/parade/reference/scaffold_batch_template.md)
  : Create a batch job template file

- [`scaffold_flow_job()`](https://bbuchsbaum.github.io/parade/reference/scaffold_flow_job.md)
  : Generate scaffold scripts for SLURM flow execution

- [`schema()`](https://bbuchsbaum.github.io/parade/reference/schema.md)
  : Alias for returns function

- [`script_await()`](https://bbuchsbaum.github.io/parade/reference/script_await.md)
  : Wait for a SLURM script job to complete

- [`script_cancel()`](https://bbuchsbaum.github.io/parade/reference/script_cancel.md)
  : Cancel a running SLURM script job

- [`script_done()`](https://bbuchsbaum.github.io/parade/reference/script_done.md)
  : Check if a SLURM job has completed

- [`script_find_latest()`](https://bbuchsbaum.github.io/parade/reference/script_find_latest.md)
  : Find the most recently created script job registries

- [`script_load()`](https://bbuchsbaum.github.io/parade/reference/script_load.md)
  : Load a script job from its registry directory

- [`script_logs()`](https://bbuchsbaum.github.io/parade/reference/script_logs.md)
  : Get log file paths for a SLURM job

- [`script_metrics()`](https://bbuchsbaum.github.io/parade/reference/script_metrics.md)
  : Get CPU and memory metrics for a SLURM job

- [`script_status()`](https://bbuchsbaum.github.io/parade/reference/script_status.md)
  : Get status of a SLURM script job

- [`script_tail()`](https://bbuchsbaum.github.io/parade/reference/script_tail.md)
  : Display recent log output from a SLURM job

- [`script_top()`](https://bbuchsbaum.github.io/parade/reference/script_top.md)
  : Interactive text monitor for a single SLURM job

- [`sink_format()`](https://bbuchsbaum.github.io/parade/reference/sink_format.md)
  : Define an inline sink format

- [`sink_quick()`](https://bbuchsbaum.github.io/parade/reference/sink_quick.md)
  : Create a sink specification quickly

- [`sink_spec()`](https://bbuchsbaum.github.io/parade/reference/sink_spec.md)
  : Create a sink specification for artifact persistence

- [`sink_temp()`](https://bbuchsbaum.github.io/parade/reference/sink_temp.md)
  : Create a temporary sink specification

- [`slurm_call()`](https://bbuchsbaum.github.io/parade/reference/slurm_call.md)
  : Submit an R function to SLURM

- [`slurm_cluster_plan()`](https://bbuchsbaum.github.io/parade/reference/slurm_cluster_plan.md)
  : Plan packed chunking for a fixed-size SLURM cluster

- [`slurm_defaults_get()`](https://bbuchsbaum.github.io/parade/reference/slurm_defaults_get.md)
  : Get defaults for SLURM (merged from options() and config)

- [`slurm_defaults_set()`](https://bbuchsbaum.github.io/parade/reference/slurm_defaults_set.md)
  : Set defaults for SLURM (R session and optionally persist to config)

- [`slurm_map()`](https://bbuchsbaum.github.io/parade/reference/slurm_map.md)
  : Map a function or script over elements via SLURM

- [`slurm_map_cluster()`](https://bbuchsbaum.github.io/parade/reference/slurm_map_cluster.md)
  : Map over tasks as if you had one big machine (best-effort)

- [`slurm_pmap()`](https://bbuchsbaum.github.io/parade/reference/slurm_pmap.md)
  : Parallel map over multiple lists/vectors via SLURM

- [`slurm_resources()`](https://bbuchsbaum.github.io/parade/reference/slurm_resources.md)
  : Build SLURM resources with defaults and normalization

- [`slurm_template()`](https://bbuchsbaum.github.io/parade/reference/slurm_template.md)
  : Get path to default SLURM template

- [`slurm_template_default()`](https://bbuchsbaum.github.io/parade/reference/slurm_template_default.md)
  : Get the default SLURM template path

- [`slurm_template_set()`](https://bbuchsbaum.github.io/parade/reference/slurm_template_set.md)
  : Set the default SLURM template path

- [`stage()`](https://bbuchsbaum.github.io/parade/reference/stage.md) :
  Add a processing stage to a parade flow

- [`status()`](https://bbuchsbaum.github.io/parade/reference/status.md)
  : Get status of all jobs in a jobset

- [`stem()`](https://bbuchsbaum.github.io/parade/reference/stem.md) :
  Generate job names from file stems

- [`struct()`](https://bbuchsbaum.github.io/parade/reference/struct.md)
  : Alias for pack function

- [`` `[`( ``*`<parade_jobset>`*`)`](https://bbuchsbaum.github.io/parade/reference/sub-.parade_jobset.md)
  : Extract subset of jobs

- [`submit()`](https://bbuchsbaum.github.io/parade/reference/submit.md)
  : Submit a flow for deferred execution

- [`submit_slurm()`](https://bbuchsbaum.github.io/parade/reference/submit_slurm.md)
  : Submit an R script to SLURM or run locally

- [`tail(`*`<parade_jobset>`*`)`](https://bbuchsbaum.github.io/parade/reference/tail.parade_jobset.md)
  : Get tail of logs for jobs

- [`tbl()`](https://bbuchsbaum.github.io/parade/reference/tbl.md) :
  Create a tibble/data frame type specification

- [`time()`](https://bbuchsbaum.github.io/parade/reference/time.md) :
  Set time limit for a resource profile

- [`top()`](https://bbuchsbaum.github.io/parade/reference/top.md) :
  Launch interactive monitor for jobset

- [`use_mirai_local()`](https://bbuchsbaum.github.io/parade/reference/use_mirai_local.md)
  : Use local mirai daemons

- [`use_mirai_slurm()`](https://bbuchsbaum.github.io/parade/reference/use_mirai_slurm.md)
  : Use SLURM-managed mirai daemons

- [`use_mirai_ssh()`](https://bbuchsbaum.github.io/parade/reference/use_mirai_ssh.md)
  : Use SSH-tunneled mirai daemons

- [`use_registry()`](https://bbuchsbaum.github.io/parade/reference/use_registry.md)
  : Get or set active registry

- [`with_parade_options()`](https://bbuchsbaum.github.io/parade/reference/with_parade_options.md)
  : Temporarily set parade options for code execution
