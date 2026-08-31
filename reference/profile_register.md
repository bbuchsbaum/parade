# Register a named resource profile

Store a resource profile in the registry for reuse across jobs. Profiles
can be retrieved by name and used as base profiles or referenced by
string shorthand. Site-specific profiles are supplied by users and
projects; parade does not embed site names or site resource values.

## Usage

``` r
profile_register(name, profile, overwrite = FALSE, persist = FALSE)
```

## Arguments

- name:

  Name for the profile

- profile:

  Resource profile object or list

- overwrite:

  Whether to overwrite an existing in-session profile

- persist:

  Whether to save the profile in the user-managed parade config so it is
  available in future R sessions. Lists may include package-side safety
  metadata `whole_node` and `cores_per_node`.

## Value

Invisible NULL

## Examples

``` r
# \donttest{
# Register a standard compute profile
standard <- profile()
standard <- res_time(standard, "4:00:00")
standard <- mem(standard, "8G")
standard <- cpus(standard, 4)
profile_register("standard", standard, overwrite = TRUE)

# Register a GPU profile
gpu <- profile()
gpu <- res_time(gpu, "12:00:00")
gpu <- mem(gpu, "32G")
gpu <- cpus(gpu, 8)
gpu <- gpus(gpu, 1)
profile_register("gpu", gpu, overwrite = TRUE)

# Use registered profiles (SLURM only; skip if not available)
if (Sys.which("squeue") != "") {
  my_function <- function(x) x + 1
  job <- slurm_call(my_function, x = 1, resources = "gpu")
}
# }
```
