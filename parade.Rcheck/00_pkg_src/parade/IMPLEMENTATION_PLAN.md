# Parade Unified API Implementation Plan

## Overview
Unifying script and function submission with consistent syntactic sugar across parade v0.12.0+

## Current Status
- [x] Basic `slurm_call()` implementation (R/slurm_call.R)
- [x] Tests for `slurm_call()` (tests/testthat/test-slurm_call.R)
- [x] Documentation in vignette (vignettes/parade-scripts-monitoring.Rmd)

---

## Phase 1: Core Infrastructure (Foundation)

### Enhanced slurm_call()
- [x] Add `name_by` parameter for dynamic naming (stem, index, digest)
- [ ] Add package auto-detection via `codetools::findGlobals()`
- [x] Add `engine = c("slurm", "local")` for local execution
- [x] Implement path macro expansion in `write_result` (`{stem}`, `{run}`, `{name}`)
- [ ] Add `guard_packages()` for explicit package validation

### Argument Helpers
- [x] Create `args_cli()` - Build character vectors for scripts
- [x] Create `args_call()` - Build named lists for functions
- [x] Create `args()` - Auto-detect based on context
- [x] Add tests for argument helpers
- [x] Document argument helper usage

### Unified Job Object
- [x] Create `parade_job` S3 class as parent class
- [x] Refactor `parade_script_job` to inherit from `parade_job`
- [x] Add `job_type` field ("script" or "function")
- [x] Implement consistent `print.parade_job()` method
- [ ] Implement `[.parade_job` subsetting method
- [ ] Implement `c.parade_job` combining method

---

## Phase 2: Map Functions (High-Value) ✅

### Core Map Implementation
- [x] Create `R/slurm_map.R` file
- [x] Implement `slurm_map()` with type dispatch
  - [x] String path → `submit_slurm()`
  - [x] Function → `slurm_call()`
  - [x] Formula notation support (`~ .x + 1`)
- [x] Implement `slurm_pmap()` for parallel mapping
- [x] Create `parade_jobset` S3 class
- [ ] Add `as_jobset()` coercion function

### Jobset Methods
- [x] `await.parade_jobset()` - Wait for all jobs
- [x] `status.parade_jobset()` - Get status of all jobs
- [ ] `progress.parade_jobset()` - Show progress bar
- [x] `cancel.parade_jobset()` - Cancel all jobs
- [x] `tail.parade_jobset()` - Tail logs from jobs
- [x] `top.parade_jobset()` - Interactive monitor
- [x] `collect.parade_jobset()` - Gather results
- [x] `print.parade_jobset()` - Pretty printing
- [x] `[.parade_jobset` - Subsetting
- [x] `length.parade_jobset()` - Number of jobs
- [x] `c.parade_jobset()` - Combining jobsets
- [x] `as_tibble.parade_jobset()` - Convert to tibble
- [x] `completed.parade_jobset()` - Filter completed jobs
- [x] `failed.parade_jobset()` - Filter failed jobs
- [x] `running.parade_jobset()` - Filter running jobs
- [x] `pending.parade_jobset()` - Filter pending jobs

### Name Helpers
- [x] `stem()` - Extract file stem for naming
- [x] `index()` - Use numeric index
- [x] `digest()` - Use content hash
- [x] `glue_name()` - Template-based naming

---

## Phase 3: Resource Management (Ergonomics) ✅

### Resource Profiles
- [x] Create `R/resource_profiles.R` file
- [x] Implement `profile()` function
- [x] Chain modifiers: `time()`, `mem()`, `cpus()`, `gpus()`
- [x] String shorthand resolution (e.g., "cpu8" → profile)
- [x] Profile validation and merging logic
- [x] Additional modifiers: `partition()`, `account()`

### Profile Registry
- [x] `profile_register()` - Define named profiles
- [x] `profile_list()` - Show available profiles
- [x] `profile_get()` - Retrieve profile definition
- [x] `profile_remove()` - Remove profiles
- [x] `profile_clear()` - Clear all profiles
- [x] `profile_init_defaults()` - Initialize default profiles
- [x] Store profiles in internal environment
- [x] Add profile examples to vignette
- [x] Integration with `slurm_resources()`

---

## Phase 4: Flow Control (Advanced) ✅

### Wave Execution
- [x] Create `R/waves.R` file
- [x] `in_waves_of()` - Submit in batches
- [x] `max_in_flight()` - Limit concurrent jobs
- [x] Internal queue management with `apply_waves()`
- [x] Concurrency control with `apply_concurrency_limit()`
- [x] Flow control combination with `flow_control()`
- [x] Tests for wave execution

### Error Handling
- [x] Create `R/error_policy.R` file
- [x] `on_error()` policy builder
- [x] `on_error_retry()` shorthand
- [x] `retry()` method with backoff (linear, exponential)
- [x] `calculate_backoff()` for delay computation
- [x] Error collection with `get_errors()`
- [x] Integration with jobset methods
- [x] Tests for error policies

### Grid Expansion
- [x] Create `R/grid.R` file with `grid()` helper
- [x] Parameter filtering with formulas and functions
- [x] `param_grid()` alternative interface
- [x] `lhs_grid()` for Latin hypercube sampling
- [x] `combine_grids()` for multi-resolution exploration
- [x] Integration with `slurm_map()` (via .options and .error_policy)
- [x] Tests for grid expansion

---

## Phase 5: Monitoring & Diagnostics (Polish) ✅

### Enhanced Monitoring
- [x] `open_logs()` - Quick log file access
- [x] `explain()` - Show what will be executed
- [x] `dry_run()` - Preview without submission
- [x] `jobs_top()` enhancement for jobsets
- [x] Registry management functions
  - [x] `use_registry()`
  - [x] `registry_ls()`
  - [x] `registry_clean()`

### Path Macros
- [x] Create `path` object with methods
- [x] `path$artifacts()` implementation
- [x] `path$data()` implementation
- [x] `path$registry()` implementation
- [x] Macro expansion: `{stem}`, `{name}`, `{run}`, `{date}`
- [x] Integration with existing `resolve_path()`
- [x] Tests for path macros

---

## Phase 6: Flow DSL (Optional Future)

### Stage-based Workflows
- [ ] Create `R/flow.R` file
- [ ] `flow()` constructor
- [ ] `stage()` method for adding stages
- [ ] `distribute()` for execution strategy
- [ ] Stage dependency resolution
- [ ] Tests for flow DSL

---

## Documentation & Examples

### Vignettes
- [ ] Create "parade-unified-api.Rmd" vignette
- [ ] Update existing vignettes with new patterns
- [ ] Add comparison table (old vs new API)
- [ ] Performance tuning guide
- [ ] Migration guide from v0.11 to v0.12

### Examples
- [ ] Parameter sweep example
- [ ] Image processing pipeline
- [ ] Monte Carlo simulation
- [ ] Cross-validation workflow
- [ ] Error recovery patterns

---

## Testing Strategy

### Unit Tests
- [x] Test file: test-slurm_map.R
- [ ] Test file: test-jobset.R
- [x] Test file: test-resource_profiles.R
- [x] Test file: test-flow_control.R (waves, errors, grids)
- [ ] Test file: test-path_macros.R

### Integration Tests
- [ ] End-to-end workflow tests
- [ ] Resource profile integration
- [ ] Wave execution with real delays
- [ ] Error recovery scenarios
- [ ] Result collection validation

---

## Release Checklist

### Before Release
- [ ] All tests passing
- [ ] R CMD check clean
- [ ] Documentation complete
- [ ] Vignettes build successfully
- [ ] NEWS.md updated
- [ ] Version bumped to 0.12.0

### Compatibility
- [ ] Backward compatibility verified
- [ ] Migration guide written
- [ ] Deprecation warnings added (if needed)
- [ ] Examples updated

---

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

---

## Notes

### Design Principles
1. **Backward Compatibility**: Existing code must continue working
2. **Progressive Disclosure**: Simple things simple, complex things possible
3. **Consistent Semantics**: Same behavior for scripts and functions
4. **Type Safety**: Clear distinction between CLI and function arguments
5. **Ergonomics First**: Optimize for common use cases

### Key Decisions Made
- `slurm_call()` already implemented as foundation ✓
- Unified job object hierarchy chosen
- Formula notation (`~`) for anonymous functions
- String shortcuts for resource profiles
- Path macros for common patterns

### Open Questions
- [ ] Should `collect()` simplify by default or return list?
- [ ] How to handle large closure serialization warnings?
- [ ] Should wave execution be synchronous or async by default?
- [ ] Integration with future/promises for async patterns?
- [ ] CLI interface priority?

### Dependencies to Consider
- `codetools` for package detection
- `glue` for string interpolation (already imported)
- `cli` for progress bars (optional)
- `withr` for temporary state management

---

## Current Focus
**Active**: Implementation Complete - Ready for documentation and examples

**Next**: Phase 6 - Flow DSL (Optional Future)

**Blocked**: None

**Completed**: 
- Phase 1: Core Infrastructure ✓
  - Enhanced slurm_call() with name_by and engine support ✓
  - Argument helpers (args_cli, args_call, args) ✓
  - Unified job class hierarchy ✓
  - Test suite for slurm_call() ✓
- Phase 2: Map Functions ✓
  - slurm_map() and slurm_pmap() implementation ✓
  - Complete jobset class with 15+ methods ✓
  - Name helpers (stem, index, digest, glue_name) ✓
  - Test suite for map functions ✓
  - Vignette documentation ✓
- Phase 3: Resource Management ✓
  - Resource profile system with fluent interface ✓
  - Profile registry with defaults ✓
  - Chain modifiers (time, mem, cpus, gpus, partition, account) ✓
  - Integration with slurm_resources() ✓
  - Test suite for profiles ✓
  - Documentation and examples ✓
- Phase 4: Flow Control ✓
  - Wave execution with in_waves_of() and max_in_flight() ✓
  - Error policies with retry and backoff strategies ✓
  - Grid expansion for parameter sweeps ✓
  - Latin hypercube sampling ✓
  - Integration with slurm_map() ✓
  - Test suite for flow control ✓
- Phase 5: Monitoring & Diagnostics ✓
  - Enhanced monitoring functions (explain, dry_run, open_logs) ✓
  - Registry management (use_registry, registry_ls, registry_clean) ✓
  - Path macro system with convenient accessors ✓
  - Enhanced path expansion with templates ✓
  - Test suite for monitoring features ✓