# Parade Unified API Implementation Plan

## Overview

Unifying script and function submission with consistent syntactic sugar
across parade v0.12.0+

## Current Status

Basic
[`slurm_call()`](https://bbuchsbaum.github.io/parade/reference/slurm_call.md)
implementation (R/slurm_call.R)

Tests for
[`slurm_call()`](https://bbuchsbaum.github.io/parade/reference/slurm_call.md)
(tests/testthat/test-slurm_call.R)

Documentation in vignette (vignettes/parade-scripts-monitoring.Rmd)

------------------------------------------------------------------------

## Phase 1: Core Infrastructure (Foundation)

### Enhanced slurm_call()

Add `name_by` parameter for dynamic naming (stem, index, digest)

Add package auto-detection via
[`codetools::findGlobals()`](https://rdrr.io/pkg/codetools/man/findGlobals.html)

Add `engine = c("slurm", "local")` for local execution

Implement path macro expansion in `write_result` (`{stem}`, `{run}`,
[name](https://github.com/christopherkenny/name))

Add
[`guard_packages()`](https://bbuchsbaum.github.io/parade/reference/guard_packages.md)
for explicit package validation

### Argument Helpers

Create
[`args_cli()`](https://bbuchsbaum.github.io/parade/reference/args_cli.md) -
Build character vectors for scripts

Create
[`args_call()`](https://bbuchsbaum.github.io/parade/reference/args_call.md) -
Build named lists for functions

Create
[`args()`](https://bbuchsbaum.github.io/parade/reference/args.md) -
Auto-detect based on context

Add tests for argument helpers

Document argument helper usage

### Unified Job Object

Create `parade_job` S3 class as parent class

Refactor `parade_script_job` to inherit from `parade_job`

Add `job_type` field (“script” or “function”)

Implement consistent
[`print.parade_job()`](https://bbuchsbaum.github.io/parade/reference/print.parade_job.md)
method

Implement `[.parade_job` subsetting method

Implement `c.parade_job` combining method

------------------------------------------------------------------------

## Phase 2: Map Functions (High-Value) ✅

### Core Map Implementation

Create `R/slurm_map.R` file

Implement
[`slurm_map()`](https://bbuchsbaum.github.io/parade/reference/slurm_map.md)
with type dispatch

String path →
[`submit_slurm()`](https://bbuchsbaum.github.io/parade/reference/submit_slurm.md)

Function →
[`slurm_call()`](https://bbuchsbaum.github.io/parade/reference/slurm_call.md)

Formula notation support (`~ .x + 1`)

Implement
[`slurm_pmap()`](https://bbuchsbaum.github.io/parade/reference/slurm_pmap.md)
for parallel mapping

Create `parade_jobset` S3 class

Add
[`as_jobset()`](https://bbuchsbaum.github.io/parade/reference/as_jobset.md)
coercion function

### Jobset Methods

[`await.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/await.md) -
Wait for all jobs

`status.parade_jobset()` - Get status of all jobs

[`progress.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/progress.md) -
Show progress bar

`cancel.parade_jobset()` - Cancel all jobs

[`tail.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/tail.parade_jobset.md) -
Tail logs from jobs

[`top.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/top.md) -
Interactive monitor

[`collect.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/collect.md) -
Gather results

[`print.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/print.parade_jobset.md) -
Pretty printing

`[.parade_jobset` - Subsetting

[`length.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/length.parade_jobset.md) -
Number of jobs

[`c.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/c.parade_jobset.md) -
Combining jobsets

[`as_tibble.parade_jobset()`](https://bbuchsbaum.github.io/parade/reference/as_tibble.parade_jobset.md) -
Convert to tibble

`completed.parade_jobset()` - Filter completed jobs

`failed.parade_jobset()` - Filter failed jobs

`running.parade_jobset()` - Filter running jobs

`pending.parade_jobset()` - Filter pending jobs

### Name Helpers

[`stem()`](https://bbuchsbaum.github.io/parade/reference/stem.md) -
Extract file stem for naming

[`index()`](https://bbuchsbaum.github.io/parade/reference/index.md) -
Use numeric index

[`digest()`](https://bbuchsbaum.github.io/parade/reference/digest.md) -
Use content hash

[`glue_name()`](https://bbuchsbaum.github.io/parade/reference/glue_name.md) -
Template-based naming

------------------------------------------------------------------------

## Phase 3: Resource Management (Ergonomics) ✅

### Resource Profiles

Create `R/resource_profiles.R` file

Implement
[`profile()`](https://bbuchsbaum.github.io/parade/reference/profile.md)
function

Chain modifiers:
[`time()`](https://bbuchsbaum.github.io/parade/reference/time.md),
[`mem()`](https://bbuchsbaum.github.io/parade/reference/mem.md),
[`cpus()`](https://bbuchsbaum.github.io/parade/reference/cpus.md),
[`gpus()`](https://bbuchsbaum.github.io/parade/reference/gpus.md)

String shorthand resolution (e.g., “cpu8” → profile)

Profile validation and merging logic

Additional modifiers:
[`partition()`](https://bbuchsbaum.github.io/parade/reference/partition.md),
[`account()`](https://bbuchsbaum.github.io/parade/reference/account.md)

### Profile Registry

[`profile_register()`](https://bbuchsbaum.github.io/parade/reference/profile_register.md) -
Define named profiles

[`profile_list()`](https://bbuchsbaum.github.io/parade/reference/profile_list.md) -
Show available profiles

[`profile_get()`](https://bbuchsbaum.github.io/parade/reference/profile_get.md) -
Retrieve profile definition

[`profile_remove()`](https://bbuchsbaum.github.io/parade/reference/profile_remove.md) -
Remove profiles

[`profile_clear()`](https://bbuchsbaum.github.io/parade/reference/profile_clear.md) -
Clear all profiles

[`profile_init_defaults()`](https://bbuchsbaum.github.io/parade/reference/profile_init_defaults.md) -
Initialize default profiles

Store profiles in internal environment

Add profile examples to vignette

Integration with
[`slurm_resources()`](https://bbuchsbaum.github.io/parade/reference/slurm_resources.md)

------------------------------------------------------------------------

## Phase 4: Flow Control (Advanced) ✅

### Wave Execution

Create `R/waves.R` file

[`in_waves_of()`](https://bbuchsbaum.github.io/parade/reference/in_waves_of.md) -
Submit in batches

[`max_in_flight()`](https://bbuchsbaum.github.io/parade/reference/max_in_flight.md) -
Limit concurrent jobs

Internal queue management with
[`apply_waves()`](https://bbuchsbaum.github.io/parade/reference/apply_waves.md)

Concurrency control with
[`apply_concurrency_limit()`](https://bbuchsbaum.github.io/parade/reference/apply_concurrency_limit.md)

Flow control combination with
[`flow_control()`](https://bbuchsbaum.github.io/parade/reference/flow_control.md)

Tests for wave execution

### Error Handling

Create `R/error_policy.R` file

[`on_error()`](https://bbuchsbaum.github.io/parade/reference/on_error.md)
policy builder

[`on_error_retry()`](https://bbuchsbaum.github.io/parade/reference/on_error_retry.md)
shorthand

[`retry()`](https://bbuchsbaum.github.io/parade/reference/retry.md)
method with backoff (linear, exponential)

[`calculate_backoff()`](https://bbuchsbaum.github.io/parade/reference/calculate_backoff.md)
for delay computation

Error collection with
[`get_errors()`](https://bbuchsbaum.github.io/parade/reference/get_errors.md)

Integration with jobset methods

Tests for error policies

### Grid Expansion

Create `R/grid.R` file with
[`grid()`](https://bbuchsbaum.github.io/parade/reference/grid.md) helper

Parameter filtering with formulas and functions

[`param_grid()`](https://bbuchsbaum.github.io/parade/reference/param_grid.md)
alternative interface

[`lhs_grid()`](https://bbuchsbaum.github.io/parade/reference/lhs_grid.md)
for Latin hypercube sampling

[`combine_grids()`](https://bbuchsbaum.github.io/parade/reference/combine_grids.md)
for multi-resolution exploration

Integration with
[`slurm_map()`](https://bbuchsbaum.github.io/parade/reference/slurm_map.md)
(via .options and .error_policy)

Tests for grid expansion

------------------------------------------------------------------------

## Phase 5: Monitoring & Diagnostics (Polish) ✅

### Enhanced Monitoring

[`open_logs()`](https://bbuchsbaum.github.io/parade/reference/open_logs.md) -
Quick log file access

[`explain()`](https://bbuchsbaum.github.io/parade/reference/explain.md) -
Show what will be executed

[`dry_run()`](https://bbuchsbaum.github.io/parade/reference/dry_run.md) -
Preview without submission

[`jobs_top()`](https://bbuchsbaum.github.io/parade/reference/jobs_top.md)
enhancement for jobsets

Registry management functions

[`use_registry()`](https://bbuchsbaum.github.io/parade/reference/use_registry.md)

[`registry_ls()`](https://bbuchsbaum.github.io/parade/reference/registry_ls.md)

[`registry_clean()`](https://bbuchsbaum.github.io/parade/reference/registry_clean.md)

### Path Macros

Create `path` object with methods

`path$artifacts()` implementation

`path$data()` implementation

`path$registry()` implementation

Macro expansion: `{stem}`,
[name](https://github.com/christopherkenny/name), `{run}`, `{date}`

Integration with existing
[`resolve_path()`](https://bbuchsbaum.github.io/parade/reference/resolve_path.md)

Tests for path macros

------------------------------------------------------------------------

## Phase 6: Flow DSL (Optional Future)

### Stage-based Workflows

Create `R/flow.R` file

[`flow()`](https://bbuchsbaum.github.io/parade/reference/flow.md)
constructor

[`stage()`](https://bbuchsbaum.github.io/parade/reference/stage.md)
method for adding stages

[`distribute()`](https://bbuchsbaum.github.io/parade/reference/distribute.md)
for execution strategy

Stage dependency resolution

Tests for flow DSL

------------------------------------------------------------------------

## Documentation & Examples

### Vignettes

Create “parade-unified-api.Rmd” vignette

Update existing vignettes with new patterns

Add comparison table (old vs new API)

Performance tuning guide

Migration guide from v0.11 to v0.12

### Examples

Parameter sweep example

Image processing pipeline

Monte Carlo simulation

Cross-validation workflow

Error recovery patterns

------------------------------------------------------------------------

## Testing Strategy

### Unit Tests

Test file: test-slurm_map.R

Test file: test-jobset.R

Test file: test-resource_profiles.R

Test file: test-flow_control.R (waves, errors, grids)

Test file: test-path_macros.R

### Integration Tests

End-to-end workflow tests

Resource profile integration

Wave execution with real delays

Error recovery scenarios

Result collection validation

------------------------------------------------------------------------

## Release Checklist

### Before Release

All tests passing

R CMD check clean

Documentation complete

Vignettes build successfully

NEWS.md updated

Version bumped to 0.12.0

### Compatibility

Backward compatibility verified

Migration guide written

Deprecation warnings added (if needed)

Examples updated

------------------------------------------------------------------------

## Timeline

### Week 1 (Current)

- Complete Phase 1 infrastructure
- Begin Phase 2 map functions

### Week 2

- Complete Phase 2 map functions
- Begin Phase 3 resource management

### Week 3

- Complete Phase 3 resource management
- Begin Phase 4 flow control

### Week 4

- Complete Phase 4 flow control
- Phase 5 monitoring & diagnostics
- Documentation and testing

### Week 5 (Buffer)

- Polish and refinement
- Performance optimization
- Release preparation

------------------------------------------------------------------------

## Notes

### Design Principles

1.  **Backward Compatibility**: Existing code must continue working
2.  **Progressive Disclosure**: Simple things simple, complex things
    possible
3.  **Consistent Semantics**: Same behavior for scripts and functions
4.  **Type Safety**: Clear distinction between CLI and function
    arguments
5.  **Ergonomics First**: Optimize for common use cases

### Key Decisions Made

- [`slurm_call()`](https://bbuchsbaum.github.io/parade/reference/slurm_call.md)
  already implemented as foundation ✓
- Unified job object hierarchy chosen
- Formula notation (`~`) for anonymous functions
- String shortcuts for resource profiles
- Path macros for common patterns

### Open Questions

Should
[`collect()`](https://bbuchsbaum.github.io/parade/reference/collect.md)
simplify by default or return list?

How to handle large closure serialization warnings?

Should wave execution be synchronous or async by default?

Integration with future/promises for async patterns?

CLI interface priority?

### Dependencies to Consider

- `codetools` for package detection
- `glue` for string interpolation (already imported)
- `cli` for progress bars (optional)
- `withr` for temporary state management

------------------------------------------------------------------------

## Current Focus

**Active**: Implementation Complete - Ready for documentation and
examples

**Next**: Phase 6 - Flow DSL (Optional Future)

**Blocked**: None

**Completed**: - Phase 1: Core Infrastructure ✓ - Enhanced slurm_call()
with name_by and engine support ✓ - Argument helpers (args_cli,
args_call, args) ✓ - Unified job class hierarchy ✓ - Test suite for
slurm_call() ✓ - Phase 2: Map Functions ✓ - slurm_map() and slurm_pmap()
implementation ✓ - Complete jobset class with 15+ methods ✓ - Name
helpers (stem, index, digest, glue_name) ✓ - Test suite for map
functions ✓ - Vignette documentation ✓ - Phase 3: Resource Management
✓ - Resource profile system with fluent interface ✓ - Profile registry
with defaults ✓ - Chain modifiers (time, mem, cpus, gpus, partition,
account) ✓ - Integration with slurm_resources() ✓ - Test suite for
profiles ✓ - Documentation and examples ✓ - Phase 4: Flow Control ✓ -
Wave execution with in_waves_of() and max_in_flight() ✓ - Error policies
with retry and backoff strategies ✓ - Grid expansion for parameter
sweeps ✓ - Latin hypercube sampling ✓ - Integration with slurm_map() ✓ -
Test suite for flow control ✓ - Phase 5: Monitoring & Diagnostics ✓ -
Enhanced monitoring functions (explain, dry_run, open_logs) ✓ - Registry
management (use_registry, registry_ls, registry_clean) ✓ - Path macro
system with convenient accessors ✓ - Enhanced path expansion with
templates ✓ - Test suite for monitoring features ✓
