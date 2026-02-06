# Initialize parade for HPC use

A one-command setup helper for running parade on HPC clusters. This
function:

- Initializes path aliases in `"hpc"` mode (prefer shared scratch over
  node-local tmp).

- Optionally creates required directories and runs a quick doctor check.

- Optionally scaffolds a batchtools SLURM template and persists it to
  `parade.json`.

- Optionally sets SLURM defaults and persists them to `parade.json`.

## Usage

``` r
parade_init_hpc(
  scratch = NULL,
  artifacts = NULL,
  registry = NULL,
  create = TRUE,
  persist = TRUE,
  template = TRUE,
  template_path = "project://batchtools/parade-slurm.tmpl",
  overwrite_template = FALSE,
  slurm_defaults = NULL,
  quiet = FALSE
)
```

## Arguments

- scratch:

  Optional shared scratch root. If `NULL`, parade auto-detects using
  `PARADE_SCRATCH` / `SCRATCH` / scheduler variables.

- artifacts:

  Optional artifacts root. Default is
  `file.path(scratch, "parade-artifacts")`.

- registry:

  Optional registry root. Default is
  `file.path(scratch, "parade-registry")`.

- create:

  Whether to create missing directories for configured roots.

- persist:

  Whether to persist paths/template/defaults to `parade.json`.

- template:

  Whether to scaffold a SLURM batchtools template file.

- template_path:

  Where to write the template (default:
  `"project://batchtools/parade-slurm.tmpl"`).

- overwrite_template:

  Whether to overwrite an existing template file.

- slurm_defaults:

  Optional named list of defaults for SLURM resources (e.g.,
  `list(mem = NA, time = "2h")`).

- quiet:

  Whether to suppress messages.

## Value

A list containing the configured `paths`, config path (if persisted),
template path (if created), and doctor results.

## Examples

``` r
# \donttest{
# Minimal HPC init (best-effort autodetect)
parade_init_hpc(quiet = TRUE, persist = FALSE, template = FALSE, create = FALSE)

# With explicit scratch + a temp template path (safe outside a cluster)
parade_init_hpc(
  scratch = tempdir(),
  persist = FALSE,
  create = FALSE,
  template = TRUE,
  template_path = tempfile(fileext = ".tmpl"),
  overwrite_template = TRUE
)
#> parade_init_hpc
#> --------------
#> - Paths initialized (profile='hpc')
#> - Project:   /home/runner/work/parade/parade/docs/reference
#> - Scratch:   /tmp/RtmpjoVM1G
#> - Artifacts: /tmp/RtmpjoVM1G/parade-artifacts
#> - Registry:  /tmp/RtmpjoVM1G/parade-registry
#> - Template:  /tmp/RtmpjoVM1G/file1bb7282fdc15.tmpl
#> 
#> Warnings
#> --------
#> - Missing directory for: data, config, cache 
# }
```
