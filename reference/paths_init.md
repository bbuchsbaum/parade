# Initialize parade path configuration

Sets up standard directory paths for parade projects, automatically
detecting HPC environments and configuring appropriate scratch and data
directories.

## Usage

``` r
paths_init(profile = c("auto", "local", "hpc"), create = FALSE, quiet = FALSE)
```

## Arguments

- profile:

  Path profile: "auto" (default), "local", or "hpc"

- create:

  Whether to create missing directories for configured roots (never
  creates `project`)

- quiet:

  Whether to suppress initialization messages

## Value

Named list of configured paths (invisibly)

## Details

`profile = "auto"` switches to `"hpc"` when scheduler environment
variables are detected (e.g., SLURM/PBS) or when common scratch
variables (`SCRATCH`, `WORK`, etc.) are set. On some login nodes,
scheduler variables may be absent; in that case, you can also use
`paths_init(profile = "hpc")` explicitly.

You can override defaults with environment variables such as
`PARADE_SCRATCH`, `PARADE_ARTIFACTS`, `PARADE_REGISTRY`, and
`PARADE_DATA`. Empty values are treated as unset.

## Examples

``` r
paths_init(profile = "local")
#> parade paths: project=/home/runner/work/parade/parade/docs/reference; scratch=/tmp/RtmpUllyCd; data=/home/runner/work/parade/parade/docs/reference/data; artifacts=/tmp/RtmpUllyCd/parade-artifacts; registry=/tmp/RtmpUllyCd/parade-registry; config=/home/runner/work/parade/parade/docs/reference/.parade; cache=/home/runner/.cache/R/parade
paths_init(quiet = TRUE)
```
