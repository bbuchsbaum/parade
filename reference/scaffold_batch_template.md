# Create a basic SLURM batch template

Generates a template file for batch job submission systems like SLURM.
The template is used by batchtools to submit jobs to the cluster.

## Usage

``` r
scaffold_batch_template(
  system = c("slurm"),
  out = file.path("batchtools", paste0("parade-", match.arg(system), ".tmpl")),
  modules = "R",
  exports = c(PARADE_SCRATCH = "${SLURM_TMPDIR:-${TMPDIR:-/tmp}}/parade-$SLURM_JOB_ID",
    OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1"),
  preamble = character(),
  overwrite = FALSE
)

scaffold_batch_template(
  system = c("slurm"),
  out = file.path("batchtools", paste0("parade-", match.arg(system), ".tmpl")),
  modules = "R",
  exports = c(PARADE_SCRATCH = "${SLURM_TMPDIR:-${TMPDIR:-/tmp}}/parade-$SLURM_JOB_ID",
    OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1"),
  preamble = character(),
  overwrite = FALSE
)
```

## Arguments

- system:

  Character string specifying the batch system. Currently only "slurm"
  is supported.

- out:

  Path where the template file should be written. Defaults to
  "batchtools/parade-slurm.tmpl".

- modules:

  Character vector of modules to load. Default is "R".

- exports:

  Named character vector of environment variables to export in the job
  script.

- preamble:

  Character vector of additional shell commands to include in the
  template preamble.

- overwrite:

  Logical indicating whether to overwrite an existing template file.

- path:

  Path where template should be created (temp file if NULL)

## Value

Path to created template file (invisibly)

Invisibly returns the normalized path to the created template file.

## Examples

``` r
template_path <- scaffold_batch_template()
# \donttest{
# Create a basic SLURM template
template_path <- scaffold_batch_template(
  system = "slurm",
  out = tempfile(fileext = ".tmpl")
)

# Create a template with custom modules and exports
template_path <- scaffold_batch_template(
  system = "slurm",
  modules = c("R/4.3.0", "gcc/11.2"),
  exports = c(CUSTOM_VAR = "value"),
  out = tempfile(fileext = ".tmpl")
)
# }
```
