# parade v0.11.0 — SLURM defaults (NA=omit) + profiles + jobs_top

- **NA or omit() drop resources**: `batch_resources()` now removes `NA`/`omit()` values so flags like `--mem` can be intentionally omitted per site.
- **Configurable defaults** in `parade.json`: `slurm_defaults_set()` / `slurm_defaults_get()` with optional persistence; `slurm_template_set()` to pin a default template.
- **Merged resources** via `slurm_resources()`; `submit_slurm()` uses defaults automatically.
- **jobs_top()**: multi-job live dashboard alongside `script_top()`.

Config search precedence: `PARADE_CONFIG` file → `<project>/parade.json` → `<project>/.parade/parade.json`.
