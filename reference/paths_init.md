# Initialize parade path configuration

Sets up standard directory paths for parade projects, automatically
detecting HPC environments and configuring appropriate scratch and data
directories.

## Usage

``` r
paths_init(profile = c("auto", "local", "hpc"), quiet = FALSE)
```

## Arguments

- profile:

  Path profile: "auto" (default), "local", or "hpc"

- quiet:

  Whether to suppress initialization messages

## Value

Named list of configured paths (invisibly)

## Details

`profile = "auto"` switches to `"hpc"` when scheduler environment
variables are detected (e.g., SLURM/PBS). On login nodes, those
variables may be absent; in that case, use `paths_init(profile = "hpc")`
explicitly.

You can override defaults with environment variables such as
`PARADE_SCRATCH`, `PARADE_ARTIFACTS`, `PARADE_REGISTRY`, and
`PARADE_DATA`. Empty values are treated as unset.

## Examples

``` r
paths_init(profile = "local")
#> parade paths: project=/home/runner/work/parade/parade/docs/reference; scratch=/tmp/Rtmpq6Ieyj; data=/home/runner/work/parade/parade/docs/reference/data; artifacts=/tmp/Rtmpq6Ieyj/parade-artifacts; registry=/tmp/Rtmpq6Ieyj/parade-registry; config=/home/runner/work/parade/parade/docs/reference/.parade; cache=/home/runner/.cache/R/parade
paths_init(quiet = TRUE)
```
