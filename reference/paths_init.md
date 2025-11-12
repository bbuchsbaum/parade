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

## Examples

``` r
paths_init(profile = "local")
#> parade paths: project=/home/runner/work/parade/parade/docs/reference; scratch=/tmp/Rtmp1lhwei; data=/home/runner/work/parade/parade/docs/reference/data; artifacts=/tmp/Rtmp1lhwei/parade-artifacts; registry=/tmp/Rtmp1lhwei/parade-registry; config=/home/runner/work/parade/parade/docs/reference/.parade; cache=/home/runner/.cache/R/parade
paths_init(quiet = TRUE)
```
