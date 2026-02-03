# SLURM Profiles: One Size Doesn't Fit All Jobs

> Note: Code evaluation is disabled to keep builds fast and
> cluster-agnostic. Use these snippets in an interactive session
> configured for your site.

## Prerequisites

This vignette assumes you understand: - [Basic SLURM
defaults](https://bbuchsbaum.github.io/parade/articles/parade-defaults.md) -
How to set single default configurations - [Core
concepts](https://bbuchsbaum.github.io/parade/articles/parade-core.md) -
Flow and stage basics

## The Problem: Different Jobs Need Different Resources

Your research workflow likely involves many different types of computing
tasks:

``` r
# Quick test with 5 samples - needs 30 minutes, 4 CPUs
test_job <- submit_slurm("test.R", 
  resources = list(partition = "debug", time = "30m", cpus_per_task = 4, mem = "8G"))

# Full analysis with 1000 samples - needs 8 hours, 32 CPUs  
prod_job <- submit_slurm("full_analysis.R",
  resources = list(partition = "compute", time = "8h", cpus_per_task = 32, mem = "64G"))

# GPU model training - needs 12 hours, 2 GPUs
gpu_job <- submit_slurm("train_model.R",
  resources = list(partition = "gpu", time = "12h", cpus_per_task = 16, mem = "32G", gres = "gpu:2"))
```

**This is tedious and error-prone**. You’re repeating resource
specifications everywhere, and it’s easy to accidentally submit a big
job to the debug partition or request wrong resources.

## The Solution: Named Resource Templates (Profiles)

**Profiles** let you define resource “recipes” once, then reference them
by name:

``` r
library(parade)

# Define your resource templates once
slurm_defaults_set(partition = "debug", time = "30m", cpus_per_task = 4, mem = "8G", 
                   profile = "testing", persist = TRUE)

slurm_defaults_set(partition = "compute", time = "8h", cpus_per_task = 32, mem = "64G",
                   profile = "production", persist = TRUE)

slurm_defaults_set(partition = "gpu", time = "12h", cpus_per_task = 16, mem = "32G", gres = "gpu:2",
                   profile = "gpu_training", persist = TRUE)

# Now use them anywhere with just the profile name
# Note: slurm_resources() uses named parameters: resources and profile
test_job <- submit_slurm("test.R", resources = slurm_resources(profile = "testing"))
prod_job <- submit_slurm("full_analysis.R", resources = slurm_resources(profile = "production"))
gpu_job <- submit_slurm("train_model.R", resources = slurm_resources(profile = "gpu_training"))
```

Much cleaner! And if you need to adjust resources, you change them in
one place.

## Quick Start: Setting Up Your First Profiles

### Step 1: Initialize parade

``` r
library(parade)
paths_init()  # Initialize parade's path system for artifact and registry management
```

### Step 2: Define profiles for your common job types

``` r
# Quick testing profile - for debugging and development
slurm_defaults_set(
  partition = "debug",
  time = "30m",
  cpus_per_task = 4,
  mem = "8G",
  profile = "quick",      # Name this profile "quick"
  persist = TRUE          # Save to config file
)

# Standard analysis profile - for regular jobs
slurm_defaults_set(
  partition = "compute",
  time = "4h",
  cpus_per_task = 16,
  mem = "32G",
  profile = "standard",
  persist = TRUE
)

# Heavy computation profile - for big analyses
slurm_defaults_set(
  partition = "compute",
  time = "24h",
  cpus_per_task = 64,
  mem = "128G",
  profile = "heavy",
  persist = TRUE
)
```

### Step 3: Use profiles in your workflows

``` r
# Quick test run
test_results <- flow(small_grid) |>
  stage("analyze", analyze_function) |>
  distribute(dist_slurm(resources = slurm_resources(profile = "quick")))

# Production run
production_results <- flow(full_grid) |>
  stage("analyze", analyze_function) |>
  distribute(dist_slurm(resources = slurm_resources(profile = "heavy")))
```

## Understanding Profiles vs. Defaults

Think of the relationship this way:

- **Defaults**: When you use
  [`slurm_defaults_set()`](https://bbuchsbaum.github.io/parade/reference/slurm_defaults_set.md)
  without a `profile` parameter, you’re setting the “default” profile
- **Profiles**: Named sets of SLURM parameters that you can switch
  between

In reality, “defaults” and “profiles” use the same underlying
mechanism - defaults are just the profile named “default”.

``` r
# These are equivalent - both set the "default" profile
slurm_defaults_set(partition = "compute", time = "2h")
slurm_defaults_set(partition = "compute", time = "2h", profile = "default")

# These create named profiles
slurm_defaults_set(partition = "debug", time = "30m", profile = "testing")
slurm_defaults_set(partition = "gpu", time = "8h", profile = "gpu")

# When you don't specify a profile, you get "default"
slurm_resources()                    # Uses "default" profile
slurm_resources(profile = "default") # Explicitly uses "default" profile (same as above)
slurm_resources(profile = "testing") # Uses "testing" profile
```

## Real-World Scenarios

### Scenario 1: Development → Testing → Production Pipeline

You’re developing an analysis that will eventually run on thousands of
samples:

``` r
# Development: Work with 5 samples locally
slurm_defaults_set(
  partition = "debug",
  time = "30m",
  cpus_per_task = 2,
  mem = "4G",
  profile = "dev",
  persist = TRUE
)

# Testing: Validate with 100 samples
slurm_defaults_set(
  partition = "compute",
  time = "2h",
  cpus_per_task = 8,
  mem = "16G",
  profile = "test",
  persist = TRUE
)

# Production: Process all 5000 samples
slurm_defaults_set(
  partition = "compute",
  time = "24h",
  cpus_per_task = 64,
  mem = "256G",
  profile = "prod",
  persist = TRUE
)

# Your analysis code stays the same!
run_analysis <- function(samples, profile_name) {
  flow(samples) |>
    stage("process", process_sample) |>
    stage("analyze", analyze_results) |>
    distribute(dist_slurm(resources = slurm_resources(profile = profile_name)))
}

# Progress through phases
dev_results <- run_analysis(samples[1:5], "dev")        # Quick iteration
test_results <- run_analysis(samples[1:100], "test")    # Validation
prod_results <- run_analysis(samples, "prod")           # Full run
```

### Scenario 2: Mixed Workload Types

Your project involves different types of computations:

``` r
# CPU-intensive statistical analysis
slurm_defaults_set(
  partition = "compute",
  time = "8h",
  cpus_per_task = 32,
  mem = "64G",
  profile = "stats",
  persist = TRUE
)

# Memory-intensive data preprocessing  
slurm_defaults_set(
  partition = "highmem",
  time = "4h",
  cpus_per_task = 16,
  mem = "256G",
  profile = "preprocessing",
  persist = TRUE
)

# GPU-accelerated machine learning
slurm_defaults_set(
  partition = "gpu",
  time = "12h",
  cpus_per_task = 8,
  mem = "32G",
  gres = "gpu:v100:2",  # Request 2 V100 GPUs
  profile = "ml_training",
  persist = TRUE
)

# I/O-intensive file processing
slurm_defaults_set(
  partition = "io",
  time = "6h",
  cpus_per_task = 4,
  mem = "16G",
  profile = "file_io",
  persist = TRUE
)

# Use the right profile for each task
preprocess_job <- submit_slurm("clean_data.R", 
                               resources = slurm_resources(profile = "preprocessing"))
stats_job <- submit_slurm("run_stats.R", 
                         resources = slurm_resources(profile = "stats"))
ml_job <- submit_slurm("train_model.R", 
                      resources = slurm_resources(profile = "ml_training"))
```

### Scenario 3: Cross-Cluster Portability

You work on multiple HPC systems with different configurations:

``` r
# University cluster - uses partition names
slurm_defaults_set(
  partition = "general",
  time = "4h",
  cpus_per_task = 16,
  mem = "32G",
  account = "lab_account",
  profile = "uni_cluster",
  persist = TRUE
)

# National supercomputer - uses QOS instead of partitions
slurm_defaults_set(
  partition = omit(),      # Explicitly omit partition flag (NA also works)
  qos = "normal",          # Use QOS instead
  time = "4h",
  cpus_per_task = 16,
  mem = "32G",
  account = "allocation_code",
  profile = "supercomputer",
  persist = TRUE
)
# Note: Use omit() or NA to prevent a SLURM flag from being set
# This is useful when your cluster uses different resource specifications

# Cloud HPC - different resource limits
slurm_defaults_set(
  partition = "cloud",
  time = "2h",            # Shorter time limits
  cpus_per_task = 32,     # More CPUs available
  mem = "64G",
  profile = "cloud_hpc",
  persist = TRUE
)

# Same code runs on any cluster!
run_on_cluster <- function(cluster_profile) {
  resources <- slurm_resources(profile = cluster_profile)
  submit_slurm("analysis.R", resources = resources)
}

# Choose cluster at runtime
job <- run_on_cluster("uni_cluster")     # or "supercomputer" or "cloud_hpc"
```

## Advanced Profile Management

### Overriding Profile Settings

Start with a profile but override specific settings:

``` r
# Base profile has 8 hour time limit
slurm_defaults_set(time = "8h", cpus_per_task = 32, profile = "standard", persist = TRUE)

# Override just the time for a longer job
# Note: slurm_resources() takes 'resources' as first parameter, 'profile' as second
long_job_resources <- slurm_resources(
  resources = list(time = "24h"),   # Override time
  profile = "standard"              # Keep other settings from profile
)
# Results in: time = "24h", cpus_per_task = 32 (from profile)

# Override multiple settings
custom_resources <- slurm_resources(
  resources = list(time = "2h", mem = "128G"),  # Override time and memory
  profile = "standard"                            # Keep cpus from profile
)
# Results in: time = "2h", mem = "128G", cpus_per_task = 32 (from profile)
```

### Programmatic Profile Selection

Choose profiles based on runtime conditions:

``` r
select_profile <- function(n_samples, needs_gpu = FALSE) {
  if (needs_gpu) {
    return("gpu_training")
  } else if (n_samples < 10) {
    return("quick")
  } else if (n_samples < 100) {
    return("standard")
  } else {
    return("heavy")
  }
}

# Automatically choose appropriate resources
analyze_dataset <- function(data) {
  n <- nrow(data)
  has_images <- "images" %in% names(data)
  
  profile <- select_profile(n, needs_gpu = has_images)
  message(sprintf("Using profile '%s' for %d samples", profile, n))
  
  flow(data) |>
    stage("process", process_function) |>
    distribute(resources = slurm_resources(profile = profile))
}
```

### Viewing and Testing Profiles

Check what profiles you have configured:

``` r
# Get specific profile settings
dev_settings <- slurm_defaults_get(profile = "dev")
print(dev_settings)

# Test all your profiles
test_all_profiles <- function() {
  profiles <- c("dev", "test", "prod", "gpu")
  
  for (prof in profiles) {
    cat("\nProfile:", prof, "\n")
    tryCatch({
      defaults <- slurm_defaults_get(profile = prof)
      cat("  Partition:", defaults$partition, "\n")
      cat("  Time:", defaults$time, "\n")
      cat("  CPUs:", defaults$cpus_per_task, "\n")
      cat("  Memory:", defaults$mem, "\n")
    }, error = function(e) {
      cat("  ERROR: Profile not found\n")
    })
  }
}

test_all_profiles()
```

## Configuration File Structure

Profiles are stored in your `parade.json` configuration file:

``` json
{
  "slurm": {
    "defaults": {
      "default": {
        "partition": "compute",
        "time": "2h",
        "cpus_per_task": 8
      },
      "quick": {
        "partition": "debug",
        "time": "30m",
        "cpus_per_task": 4,
        "mem": "8G"
      },
      "standard": {
        "partition": "compute",
        "time": "4h",
        "cpus_per_task": 16,
        "mem": "32G"
      },
      "heavy": {
        "partition": "compute",
        "time": "24h",
        "cpus_per_task": 64,
        "mem": "128G"
      },
      "gpu_training": {
        "partition": "gpu",
        "time": "12h",
        "cpus_per_task": 8,
        "mem": "32G",
        "gres": "gpu:2"
      }
    }
  }
}
```

Location: `<project>/.parade/parade.json` (created automatically)

## Best Practices

### 1. Use Descriptive Profile Names

``` r
# Good: Clear purpose
slurm_defaults_set(..., profile = "quick_test")
slurm_defaults_set(..., profile = "full_analysis")
slurm_defaults_set(..., profile = "gpu_training")

# Bad: Vague names
slurm_defaults_set(..., profile = "config1")
slurm_defaults_set(..., profile = "new")
```

### 2. Document Your Profiles

``` r
# Document profile purpose when creating
slurm_defaults_set(
  partition = "debug",
  time = "30m",
  cpus_per_task = 4,
  mem = "8G",
  profile = "dev",
  persist = TRUE
)
message("Created 'dev' profile for quick testing with small datasets")
```

### 3. Establish Team Conventions

Agree on standard profile names across your team: - `dev` - Development
and debugging - `test` - Testing with moderate data - `prod` -
Production runs - `gpu` - GPU-accelerated tasks - `highmem` -
Memory-intensive tasks

### 4. Handle Missing Profiles Gracefully

``` r
safe_submit <- function(script, profile_name) {
  tryCatch({
    resources <- slurm_resources(profile = profile_name)
    submit_slurm(script, resources = resources)
  }, error = function(e) {
    message(sprintf("Profile '%s' not found, using defaults", profile_name))
    submit_slurm(script)  # Falls back to default profile
  })
}
```

## Troubleshooting

## Under the Hood: where profiles live

When you call `slurm_defaults_set(...)` parade updates two places:

1.  The current R session via the `parade.slurm.defaults` option so
    subsequent
    [`slurm_resources()`](https://bbuchsbaum.github.io/parade/reference/slurm_resources.md)
    calls in the same session see the changes immediately.
2.  (Optionally) The on-disk config stored at
    `file.path(paths_get()$config, "parade.json")` when
    `persist = TRUE`. This makes the profile available to future
    sessions and collaborators who share the project.

Internally the JSON looks like:

``` json
{
  "slurm": {
    "defaults": {
      "default": {"partition": "compute", "time": "2h"},
      "gpu_training": {"partition": "gpu", "gres": "gpu:2"}
    },
    "template": "registry://templates/parade-slurm.tmpl"
  }
}
```

`slurm_resources(profile = "...")` merges the stored defaults with any
inline overrides you pass, then normalizes the result via
`batchtools::batch_resources()`. The final list travels with each
[`submit_slurm()`](https://bbuchsbaum.github.io/parade/reference/submit_slurm.md)/[`dist_slurm()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm.md)
call into batchtools’ registry, where it ultimately feeds the SLURM
template.

### Relationship to `slurm_template_set()`

[`slurm_template_set()`](https://bbuchsbaum.github.io/parade/reference/slurm_template_set.md)
merely remembers which batchtools template file to use (also inside the
same JSON config). Templates stay completely decoupled from profiles:
profiles define what resources you request, while the template dictates
*how* those resources are rendered into `#SBATCH` lines. Keeping them
separate means you can swap templates (e.g., different clusters or
accounting directives) without touching your resource presets, and vice
versa.

### Issue: “Profile not found”

``` r
# Check available profiles
slurm_defaults_get(profile = "default")  # Should always work

# Verify profile was saved
slurm_defaults_set(..., profile = "myprofile", persist = TRUE)  # Note: persist = TRUE
```

### Issue: Profile settings not applying

``` r
# Explicitly pass profile to resources
resources <- slurm_resources(profile = "production")  # Correct

# NOT just:
resources <- slurm_resources()  # Uses "default" profile
```

### Issue: Different clusters need different profiles

``` r
# Detect cluster and choose profile
get_cluster_profile <- function() {
  hostname <- Sys.info()["nodename"]
  if (grepl("uni-cluster", hostname)) {
    return("uni_profile")
  } else if (grepl("super", hostname)) {
    return("supercomputer_profile")
  } else {
    return("default")
  }
}

profile <- get_cluster_profile()
resources <- slurm_resources(profile = profile)
```

## Next Steps

Now that you understand profiles, learn about:

- [Basic SLURM
  defaults](https://bbuchsbaum.github.io/parade/articles/parade-defaults.md)
  if you haven’t already
- [Distributed
  execution](https://bbuchsbaum.github.io/parade/articles/parade-slurm-distribution.md)
  for running parallel workloads
- [Script
  monitoring](https://bbuchsbaum.github.io/parade/articles/parade-scripts-monitoring.md)
  for tracking job progress

## Quick Reference

| Function                                       | Purpose                   | Example                                                    |
|------------------------------------------------|---------------------------|------------------------------------------------------------|
| `slurm_defaults_set(..., profile = "name")`    | Create/update profile     | `slurm_defaults_set(time = "4h", profile = "standard")`    |
| `slurm_defaults_get(profile = "name")`         | Get profile settings      | `slurm_defaults_get(profile = "production")`               |
| `slurm_resources(profile = "name")`            | Use profile for resources | `slurm_resources(profile = "gpu")`                         |
| `slurm_resources(list(...), profile = "name")` | Override profile settings | `slurm_resources(list(time = "2h"), profile = "standard")` |

## Summary

Profiles transform SLURM resource management from repetitive and
error-prone to clean and maintainable:

- **Before profiles**: Repeat resource specifications everywhere
- **With profiles**: Define once, use anywhere by name
- **Key insight**: Profiles are just named sets of SLURM defaults

Start with 2-3 profiles for your common job types, and add more as
needed. Your future self (and your collaborators) will thank you!
