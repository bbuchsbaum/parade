# Repository Guidelines

## Project Structure & Modules

- `R/`: Package source (functions, S3 methods). Keep files small and
  cohesive.
- `tests/testthat/`: Unit tests (`test-*.R`) and helpers (`helper-*.R`).
- `man/`: Auto-generated Rd docs from roxygen comments.
- `vignettes/`: R Markdown articles and long-form examples.
- `inst/`: Installed extras (e.g., templates, example data).
- `DESCRIPTION`, `NAMESPACE`: Package metadata; `NAMESPACE` is
  roxygen-managed.
- `doc/`, `Meta/`: Built site artifacts (pkgdown); do not edit manually.

## Build, Test, and Dev Commands

- Build & check: `R -q -e "devtools::document(); devtools::check()"`
- Run tests: `R -q -e "devtools::test()"`
- Full CRAN check: `R CMD build . && R CMD check parade_*.tar.gz`
- Build docs site: `R -q -e "pkgdown::build_site()"`
- Lint/format: `R -q -e "lintr::lint_package(); styler::style_pkg()"`

## Coding Style & Naming

- Follow tidyverse style: 2-space indents, no tabs, 80–100 cols.
- Functions and objects: `snake_case`; S3 methods `generic.class`.
- Document with roxygen2; export via `@export`; keep examples
  runnable/fast.
- Prefer pure functions, explicit arguments, and typed schemas in
  user-facing APIs.

## Testing Guidelines

- Framework: testthat. Place tests under `tests/testthat/` as
  `test-*.R`.
- Add tests for new behavior and bug fixes; keep them deterministic and
  fast.
- Run `devtools::test()` locally; CI uses `devtools::check()` semantics.
- Use helpers for HPC/SLURM skips (see existing `helper-skip.R`).

## Commit & Pull Requests

- Commits: short, imperative, scoped (e.g., “Fix test failures”, “Add
  mirai backend”).
- PRs must include: summary, motivation, minimal repro or examples, and
  tests.
- Reference issues with `Fixes #NNN` when applicable. Include
  before/after output for CLI/text UI changes.

## Continuous Integration

- GitHub Actions: keep pkgdown workflow in
  `.github/workflows/pkgdown.yaml` for site deploys.
- Add R CMD check workflow using r-lib/actions (matrix across OSes):
  - `uses: r-lib/actions/setup-r@v2`, `setup-pandoc@v2`,
    `setup-r-dependencies@v2`, then `check-r-package@v2`.
- Cache dependencies and run `--as-cran` checks; fail on NOTES that
  matter to CRAN.

Example steps snippet:

        - uses: r-lib/actions/setup-r@v2
        - uses: r-lib/actions/setup-pandoc@v2
        - uses: r-lib/actions/setup-r-dependencies@v2
        - uses: r-lib/actions/check-r-package@v2

## Test Coverage

- Local HTML report: `R -q -e "covr::report()"` (opens coverage for the
  package).
- CI upload (optional): `R -q -e "covr::codecov()"` with a
  `CODECOV_TOKEN` secret for private repos.
- Target: maintain ≥80% coverage for user-facing code; exclude
  intentionally slow/SLURM paths (already skipped in tests).

## Security & Configuration

- Do not commit credentials or site-specific paths. Use
  [`paths_init()`](https://bbuchsbaum.github.io/parade/reference/paths_init.md)
  and path aliases (`artifacts://`, `data://`, …).
- Site defaults: prefer NA-as-omit semantics for SLURM flags; configure
  via package APIs or env vars (`PARADE_*`).
- Test locally first (`engine = "future"`), then scale out (SLURM/mirai)
  with the same code.
