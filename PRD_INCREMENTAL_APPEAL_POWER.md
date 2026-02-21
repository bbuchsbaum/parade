# PRD: Incremental Appeal and Power for `parade` (Without DNA Drift)

## Document Control
- Status: Proposed
- Owner: `parade` maintainers
- Target window: next 2 to 3 minor releases (e.g., `0.13.x` to `0.15.x`)
- Last updated: 2026-02-19

## Summary
Increase `parade` adoption and day-to-day utility by adding targeted capabilities inspired by Python workflow systems, while preserving `parade`'s core identity:
- declarative, compositional R pipelines
- typed contracts and artifact-first outputs
- laptop-to-HPC portability with strong SLURM ergonomics
- small API surface and low operational overhead

This PRD explicitly avoids platform bloat (full orchestration service, heavy control plane, scheduler rewrite).

## Problem Statement
`parade` is strong on composable dataflow and HPC ergonomics, but users still need better:
- visibility into run outcomes and failure causes
- contract clarity for cache validity and stale output detection
- artifact discoverability for downstream reuse
- resilience under intermittent task failures
- stage-level resource expression where jobs have heterogeneous needs

Without these, users either overbuild custom wrappers or migrate to heavier systems.

## Goals
1. Improve confidence and debuggability for local and cluster runs.
2. Strengthen reproducibility and cache correctness.
3. Increase artifact discoverability and portability.
4. Improve reliability for long-running HPC workflows.
5. Keep existing code working unchanged unless users opt into new features.

## Non-Goals
1. Build a hosted orchestration platform.
2. Replace existing execution engines (`future`, `mirai`, SLURM integration).
3. Introduce broad backend abstractions beyond current needs.
4. Require users to rewrite existing `flow()`/`stage()` pipelines.

## Product Principles (Guardrails)
1. Default behavior must remain backward-compatible.
2. New capability should be additive and mostly opt-in.
3. Keep concepts aligned with existing primitives (`flow`, `stage`, `schema`, sinks, `collect`, SLURM helpers).
4. Prefer simple, inspectable data artifacts (JSON/JSONL, tibbles, sidecars) over hidden state.

## Scope and Prioritization

### In Scope (Priority Order)
1. Run visibility core (`#3` from candidate list)
2. Contracts + plan/fingerprint (`#4`)
3. Artifact catalog hardening (`#5`, metadata + filtering first)
4. Reliability controls (`#1`, retries/cancel only)
5. Stage-level resource hints (`#2`, incremental SLURM mapping)

### Out of Scope for This PRD
1. Scheduling priorities and advanced dependency-aware scheduler hints.
2. Automatic chunking optimizer and aggressive memory planners.
3. Full pluggable non-SLURM backend framework.
4. Persistent live web dashboard service.

## Releases and Feature Set

## Release A: Run Visibility Core
### Features
1. Deterministic run IDs (flow-level and stage-attempt-level).
2. Standard run state model: `pending`, `running`, `completed`, `failed`, `cancelled`.
3. Structured metadata logging per run/stage attempt:
   - start/end timestamps
   - duration
   - engine and host info
   - retry attempt number
   - terminal reason/error classification
4. Minimal run summary output:
   - machine-readable JSON summary
   - optional small HTML report for quick diagnosis

### Acceptance Criteria
1. When users run existing workflows with defaults, results remain unchanged.
2. A run emits a deterministic run ID that is stable for identical flow spec + param grid + code fingerprint inputs.
3. Each stage execution emits one terminal state only; no ambiguous dual terminal states.
4. `run_summary()` (or equivalent) identifies failed stages with actionable reason and source location metadata.
5. Logging overhead remains bounded (target: no more than 5% runtime overhead in representative local test workload).

## Release B: Contracts, Fingerprints, and Plan
### Features
1. Explicit stage input/output contract declarations extending current schema semantics.
2. Dependency fingerprint/hash over stage spec + declared inputs + selected environment/version fields.
3. Stale-output detection and invalidation reason reporting.
4. `plan` / dry-run mode showing:
   - stages/tasks to execute
   - stages/tasks to reuse from cache/artifacts
   - reason codes for execute vs reuse decisions

### Acceptance Criteria
1. Existing schemas continue to work; stricter contract checks are opt-in or default-soft with clear warnings.
2. On contract violation, error messages identify stage, field, expected type/contract, and actual value class.
3. Fingerprint changes when relevant stage code/inputs change; does not change for irrelevant metadata changes.
4. Dry-run output is deterministic for identical inputs and includes explicit reason codes.
5. Users can disable strict invalidation checks for legacy workflows via documented compatibility option.

## Release C: Artifact Catalog Hardening
### Features
1. Extend artifact metadata index with:
   - creator/user
   - code/version fingerprint
   - schema signature
   - params hash
   - upstream run ID
   - run status
2. Query filters by indexed fields (not only path substring):
   - stage
   - field
   - run status
   - selected param keys
3. Run manifest export/import (JSON):
   - deterministic list of produced artifacts and metadata
   - suitable for downstream consumption and reproducibility records

### Acceptance Criteria
1. Existing catalog calls continue to work with no required argument changes.
2. New metadata fields are present for newly produced artifacts and gracefully `NA`/missing for legacy artifacts.
3. Filter queries return stable, documented columns and deterministic ordering.
4. Manifest export/import round-trip preserves artifact identity and key metadata fields.
5. Catalog operations remain performant for typical project scales (target thresholds defined in benchmark fixture).

## Release D: Reliability Controls (Retries and Cancellation)
### Features
1. Per-stage and per-flow retry policy:
   - `retries`
   - `retry_backoff` (fixed/linear/exponential)
   - `retry_on` predicate/classifier
2. Cancellation propagation mode:
   - optional fail-fast for dependent downstream stages
   - configurable behavior for independent branches

### Acceptance Criteria
1. With retries unset, behavior matches current execution semantics.
2. Retry policy applies deterministically and records attempt metadata in run logs.
3. Fail-fast cancellation stops only configured dependent work and never cancels unrelated completed stages.
4. Errors clearly indicate terminal failure after retries exhausted.
5. Retry behavior is consistent across local, `future`, and SLURM paths (subject to engine capability notes).

## Release E: Stage-Level Resource Hints
### Features
1. Optional stage-level resource hints:
   - `cpus`
   - `memory`
   - `time`
2. Resolution/inheritance order:
   - explicit stage hints
   - flow-level defaults
   - active profile/site defaults
3. SLURM mapping with clear omission behavior for `NA` fields and site policy constraints.

### Acceptance Criteria
1. No resource hints required for existing workflows; old defaults still apply.
2. Effective resolved resources are inspectable before submission.
3. SLURM script/directive generation reflects documented precedence exactly.
4. Invalid resource combinations fail early with actionable messages.
5. Existing profile/default behavior remains backward-compatible when stage hints are absent.

## Non-Regression Plan (Do Not Break Working Code)

## Compatibility Contract
1. Public API remains additive; no required argument removals or semantic inversions.
2. Existing output shapes/classes from core user-facing functions remain stable unless version-gated and documented.
3. Existing artifact sidecars remain readable.
4. Existing SLURM defaults and `NA`-as-omit behavior remain unchanged unless user opts into new per-stage hints.

## Safety Mechanisms
1. Feature flags/options for new strict behaviors (contract strictness, invalidation strictness, fail-fast).
2. Soft-launch modes:
   - warn before enforce where practical
   - document migration paths with examples
3. Versioned metadata schemas for run logs/manifests with backwards readers.

## Required Testing Gates
1. Unit tests for every new user-facing option and edge case.
2. Regression tests for representative existing workflows (local + future + SLURM-mocked paths).
3. Snapshot tests for dry-run/plan output and run summary output.
4. Backward compatibility tests:
   - legacy artifacts discoverable/readable
   - previous public function calls still succeed
5. Performance guard tests on representative workloads to detect large overhead regressions.

No release promoted to stable unless all gates pass in CI.

## UX and Documentation Requirements
1. Update vignettes with one concise end-to-end example per release feature.
2. Add migration notes for all new defaults/flags.
3. Provide troubleshooting matrix for common failures:
   - contract mismatch
   - stale cache invalidation
   - retry exhaustion
   - resource resolution conflicts

## Success Metrics
1. Reduced median time-to-diagnose failed runs (tracked via issue templates or user feedback).
2. Increased reuse rate from artifact catalog queries.
3. Reduced rerun volume caused by stale/incorrect cache reuse.
4. Increased successful completion rate for long-running HPC flows with retry policies.

## Risks and Mitigations
1. Risk: feature creep toward orchestration platform.
   - Mitigation: enforce out-of-scope list and additive minimal API.
2. Risk: metadata/logging overhead.
   - Mitigation: lightweight formats, bounded payloads, performance gate.
3. Risk: compatibility drift from stricter contracts.
   - Mitigation: opt-in strictness first, warn mode, migration docs.
4. Risk: inconsistent engine semantics.
   - Mitigation: capability matrix and engine-specific tests.

## Proposed Milestones
1. Milestone 1 (`0.13.x`): Release A complete.
2. Milestone 2 (`0.14.x`): Releases B and C core complete.
3. Milestone 3 (`0.15.x`): Releases D and E complete.

## Open Decisions
1. Which fields participate in canonical run ID and fingerprint by default?
2. Which strictness options default to `warn` vs `error` initially?
3. Minimum supported metadata schema version window for backward readers?

