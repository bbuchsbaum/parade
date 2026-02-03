# Artifacts: format-agnostic sinks, atomic writes, and manifests

> Requires **parade ≥ 0.12.0** for format-agnostic sinks (≥ 0.6.0 for
> basic sinks).

## Before you begin

- New to parade? Start with the
  [Quickstart](https://bbuchsbaum.github.io/parade/articles/articles/parade-quickstart.md)
  or the
  [Overview](https://bbuchsbaum.github.io/parade/articles/articles/parade-overview.md).
- For job-centric usage (functions/scripts), see the [Unified
  API](https://bbuchsbaum.github.io/parade/articles/articles/parade-unified-api.md).

## What are sinks and artifacts?

**Sinks** in parade provide a powerful mechanism for persisting large
computational results to disk instead of keeping them in memory. When
your stages produce big objects (models, large datasets, complex
results), sinks automatically write these to files and return
lightweight **file references** instead.

### Key benefits

- **Memory efficiency**: Large objects don’t accumulate in memory across
  pipeline stages
- **Format flexibility**: Support for RDS, CSV, JSON, Parquet, and
  custom formats
- **Persistence**: Results survive R session crashes and can be accessed
  later
- **Atomic writes**: Files are written safely without corruption risks
- **Metadata tracking**: Optional JSON sidecars with checksums (sha256)
  and byte sizes
- **Flexible organization**: Configurable directory structures and file
  naming

### Sinks vs. regular return values

| **Regular returns**                     | **Sink artifacts**                         |
|-----------------------------------------|--------------------------------------------|
| Objects stay in memory                  | Objects written to disk                    |
| Passed directly between stages          | File references passed instead             |
| Lost when R session ends                | Persist across sessions                    |
| Can cause memory issues with large data | Memory-efficient regardless of size        |
| No automatic metadata                   | Rich metadata (size, checksum, timestamps) |

## Quick start: sink_quick() for rapid prototyping

New in parade 0.12.0,
[`sink_quick()`](https://bbuchsbaum.github.io/parade/reference/sink_quick.md)
provides the fastest way to create sinks:

``` r
# One-liner sink using registered format
sink <- sink_quick("result", write = "rds")

# CSV sink with formula syntax
csv_sink <- sink_quick("data",
  write = ~ write.csv(.x, .path, row.names = FALSE),
  read = ~ read.csv(.path, stringsAsFactors = FALSE),
  ext = ".csv"
)

# Temporary sink for testing (writes to tempdir)
tmp_sink <- sink_temp("output", write = "json")

# Use in a flow
fl <- flow(grid) |>
  stage("process",
    f = function(x) list(result = process_data(x)),
    schema = returns(result = artifact()),
    sink = sink_quick("result", write = "parquet")
  )
```

### Formula syntax conventions

When using formulas with
[`sink_quick()`](https://bbuchsbaum.github.io/parade/reference/sink_quick.md),
three special variables are available: - `.x` - the object to write (the
data being saved) - `.path` - the full target path (includes file
extension, e.g., “data/output.csv”) - `.base` - the target path without
extension (e.g., “data/output”)

Example:

``` r
# Custom write function using formula variables
sink_quick("output",
  write = ~ {
    message("Writing ", class(.x)[1], " to ", .path)  # .x is your data object
    if (nrow(.x) > 1000) {
      # For large data, write to multiple files
      write.csv(.x[1:1000,], paste0(.base, "_part1.csv"))     # .base for custom names
      write.csv(.x[1001:nrow(.x),], paste0(.base, "_part2.csv"))
    } else {
      write.csv(.x, .path, row.names = FALSE)                 # .path for standard output
    }
  },
  ext = ".csv"
)
```

## Format-agnostic sinks: beyond RDS

Parade now supports **any file format** through a flexible registry
system. Choose the optimal format for your use case:

### Built-in formats

| Format      | Speed     | Size     | Type Support  | Package Required | Use Case                  |
|-------------|-----------|----------|---------------|------------------|---------------------------|
| **rds**     | Fast      | Medium   | All R objects | None             | Default, R-only workflows |
| **qs2**     | Fastest   | Smallest | All R objects | qs2              | High-performance R        |
| **parquet** | Fast      | Small    | Data frames   | arrow            | Cross-language, columnar  |
| **feather** | Very fast | Medium   | Data frames   | arrow            | Fast interchange          |
| **csv**     | Slow      | Large    | Data frames   | None             | Universal compatibility   |
| **json**    | Medium    | Large    | Lists/vectors | jsonlite         | Config, web APIs          |

### Check available formats

``` r
# List all registered formats
list_sink_formats()
# [1] "csv" "feather" "json" "parquet" "qs2" "rds" "readr_csv" "tsv"

# Check if a specific format is available
has_sink_format("parquet")  # TRUE if arrow is installed

# Get format details
fmt <- get_sink_format("parquet")
# List with writer, reader, ext, atomic fields
```

## 1) Define a sink with sink_spec()

The traditional
[`sink_spec()`](https://bbuchsbaum.github.io/parade/reference/sink_spec.md)
function now supports multiple formats:

``` r
# Single format for all fields
sink <- sink_spec(
  fields   = c("model", "metrics"),      # Which fields to persist
  dir      = "artifacts://fits",         # Base directory
  format   = "parquet",                   # Use Parquet format
  template = "{.stage}/{subject}/{session}-{.row_key}",
  overwrite = "skip"
)

# Different format per field (new!)
multi_sink <- sink_spec(
  fields = c("data", "model", "config"),
  dir = "artifacts://mixed",
  # Prefer qs2 when installed, otherwise fall back to rds
  # (keeps this vignette runnable even without qs2 installed)
  formats = {
    fast_r <- if (has_sink_format("qs2")) "qs2" else "rds"
    list(
      data = "parquet",    # Fast columnar format for data
      model = fast_r,      # Fast R serialization for models
      config = "json"      # Human-readable for configuration
    )
  }
)
```

### Key parameters explained

- **`fields`**: Character vector of output field names to persist
- **`dir`**: Base directory path (can use aliases like `artifacts://`)
- **`format`**: Single format name or custom writer function
- **`formats`**: Named list for per-field format specifications (new!)
- **`template`**: Glue template for file paths with variables:
  - `{.stage}`: Current stage name
  - `{.field}`: Field being written
  - `{.row_key}`: Unique hash of row parameters
  - Plus any columns from your parameter grid
- **`overwrite`**: “skip” (default), “overwrite”, or “error”
- **`sidecar`**: JSON metadata with checksums and timestamps

## 2) Register custom formats

Extend parade with your own formats using the registry system:

``` r
# Register a custom format globally
register_sink_format("nifti",
  writer = function(x, path, ...) {
    neuroim2::write_vol(x, path)
    invisible(path)
  },
  reader = function(path, ...) {
    neuroim2::read_vol(path)
  },
  ext = ".nii.gz",
  atomic = TRUE  # Use temp-then-rename pattern
)

# Now use it anywhere
brain_sink <- sink_quick("brain_data", write = "nifti")

# Or define inline without registration
custom_sink <- sink_quick("special",
  write = function(x, path) {
    # Your custom write logic
    my_special_writer(x, path)
  },
  read = function(path) {
    my_special_reader(path)
  },
  ext = ".custom"
)
```

### Example: HDF5 format for scientific data

``` r
# Register HDF5 support
if (requireNamespace("hdf5r", quietly = TRUE)) {
  register_sink_format("hdf5",
    writer = function(x, path, ...) {
      file <- hdf5r::H5File$new(path, mode = "w")
      tryCatch({
        file[["data"]] <- x
        invisible(path)
      }, error = function(e) {
        stop("Failed to write HDF5 file: ", e$message)
      }, finally = {
        # Ensure file is closed even if write fails
        if (file$is_valid) file$close()
      })
    },
    reader = function(path, ...) {
      file <- hdf5r::H5File$new(path, mode = "r")
      tryCatch({
        data <- file[["data"]][]
        data
      }, error = function(e) {
        stop("Failed to read HDF5 file: ", e$message)
      }, finally = {
        # Ensure file is closed even if read fails
        if (file$is_valid) file$close()
      })
    },
    ext = ".h5"
  )
}
```

## 3) Temporary sinks for development

Use
[`sink_temp()`](https://bbuchsbaum.github.io/parade/reference/sink_temp.md)
for ephemeral storage during development and testing:

``` r
# Creates sink in tempdir() with timestamp
dev_sink <- sink_temp("test_output", write = "json")

# Custom prefix for organization
test_sink <- sink_temp("experiment",
  write = "csv",
  prefix = "unittest"
)

# Perfect for iterative development
fl <- flow(small_test_data) |>
  stage("test",
    f = my_experimental_function,
    schema = returns(output = artifact()),
    sink = sink_temp("output", write = "rds")  # No cleanup needed!
  )
```

Files are written to `tempdir()/parade-quick-TIMESTAMP/`. Temporary
directories are often cleaned by the OS between sessions, but this is
not guaranteed; use `unlink(path, recursive = TRUE)` if you need
explicit cleanup.

## 4) Using sinks in stages

Attach sinks to stages and declare outputs as artifacts:

``` r
# Create parameter grid
grid <- param_grid(
  subject = c("s01", "s02", "s03"),
  session = 1:2,
  condition = c("control", "treatment")
)

# Multi-format workflow
fl <- flow(grid, seed_col = "seed") |>
  stage("process",
    f = function(subject, session, condition) {
      # Different outputs need different formats
      raw_data <- simulate_measurements(n = 10000)  # Large numeric matrix
      summary_stats <- summarize_data(raw_data)     # Small data frame
      metadata <- list(                             # Nested list
        subject = subject,
        session = session,
        condition = condition,
        timestamp = Sys.time(),
        parameters = get_analysis_params()
      )
      
      list(
        raw_data = raw_data,
        summary = summary_stats,
        metadata = metadata
      )
    },
    schema = schema(
      raw_data = artifact(),
      summary = artifact(),
      metadata = artifact()
    ),
    sink = sink_spec(
      fields = c("raw_data", "summary", "metadata"),
      dir = "artifacts://analysis",
      formats = list(
        raw_data = if (has_sink_format("qs2")) "qs2" else "rds",  # Fast for large matrices
        summary = "csv",      # Readable, shareable
        metadata = "json"     # Human-readable config
      )
    )
  )

results <- collect(fl)
```

## 5) Reading artifacts back

### Automatic loading in downstream stages

With `autoload = TRUE` (default), artifacts load automatically:

``` r
fl <- fl |>
  stage("analyze",
    f = function(raw_data, summary, metadata) {
      # All three artifacts are loaded automatically
      # in their appropriate formats
      combined <- merge_with_metadata(raw_data, metadata)
      enhanced <- enhance_summary(summary, combined)
      list(final = enhanced)
    },
    schema = returns(final = tbl())
  )
```

### Manual loading

Access artifacts directly using file references:

``` r
results <- collect(fl)

# Each artifact field contains file metadata
results$raw_data[[1]]
# tibble: path, bytes, sha256, written, existed

# Load manually using the path
data <- readRDS(results$raw_data[[1]]$path)  # For RDS
data <- read.csv(results$summary[[1]]$path)  # For CSV
data <- jsonlite::read_json(results$metadata[[1]]$path)  # For JSON
```

### Using the manifest system

Query and explore all artifacts:

``` r
# Find all Parquet files
manifest("artifacts://") |>
  filter(grepl("\\.parquet$", path)) |>
  arrange(desc(mtime))

# Load recent large files
recent_large <- manifest("artifacts://analysis") |>
  filter(
    mtime > Sys.time() - 86400,  # Last 24 hours
    bytes > 1e6                   # Larger than 1MB
  ) |>
  mutate(
    format = tools::file_ext(path),
    data = map(path, ~ {
      if (grepl("\\.qs2$", .x) && requireNamespace("qs2", quietly = TRUE)) qs2::qs_read(.x)
      else if (grepl("\\.qs$", .x) && requireNamespace("qs", quietly = TRUE)) qs::qread(.x)
      else if (grepl("\\.parquet$", .x)) arrow::read_parquet(.x)
      else readRDS(.x)
    })
  )
```

## Format selection guide

### When to use each format

**RDS (default)** - ✅ Any R object (models, lists, environments) - ✅
Preserves all R attributes and classes - ❌ R-only, not readable by
other languages - 📊 Best for: R-specific workflows, complex objects

**QS2 (when available)** - ✅ 3-5x faster than RDS - ✅ 30-50% smaller
files - ✅ Supports all R objects - ❌ Requires qs2 package - 📊 Best
for: High-performance R workflows

**Parquet** - ✅ Columnar format, excellent compression - ✅
Cross-language (Python, Julia, etc.) - ✅ Fast for data frames - ❌ Data
frames only - 📊 Best for: Data science pipelines, cross-language work

**CSV** - ✅ Universal compatibility - ✅ Human-readable - ❌ Slow,
large files - ❌ Type information lost - 📊 Best for: Sharing results,
small data

**JSON** - ✅ Human-readable - ✅ Good for nested structures - ✅ Web
API compatible - ❌ Larger files - 📊 Best for: Configuration, metadata,
web integration

## Advanced examples

### Mixed-format machine learning pipeline

``` r
ml_sink <- sink_spec(
  fields = c("train_data", "test_data", "model", "predictions", "metrics"),
  dir = "artifacts://ml_pipeline",
  formats = list(
    train_data = "parquet",   # Large, need column access
    test_data = "parquet",    # Consistent with training
    model = if (has_sink_format("qs2")) "qs2" else "rds",  # Complex R object, need speed
    predictions = "csv",      # Share with stakeholders
    metrics = "json"         # Track in version control
  ),
  template = "{.stage}/{experiment_id}/{.field}_{fold_id}"
)
```

### Scientific computing with custom formats

``` r
# Register specialized formats
register_sink_format("matlab",
  writer = ~ R.matlab::writeMat(.x, .path),
  reader = ~ R.matlab::readMat(.path),
  ext = ".mat"
)

register_sink_format("netcdf",
  writer = function(x, path) {
    ncdf4::nc_create(path, x$var_defs)
    # ... write logic
  },
  reader = ~ ncdf4::nc_open(.path),
  ext = ".nc"
)

scientific_sink <- sink_quick(
  fields = c("simulation", "observations"),
  write = "netcdf",
  dir = "artifacts://climate_model"
)
```

### Quick iteration during development

``` r
# Start with temp sink for experimentation
dev_flow <- flow(test_data) |>
  stage("experiment",
    f = my_experimental_analysis,
    schema = returns(result = artifact()),
    sink = sink_temp("result", write = "rds")
  )

# Test and iterate quickly
test_results <- collect(dev_flow)

# When ready, switch to production sink
prod_flow <- flow(full_data) |>
  stage("experiment",
    f = my_experimental_analysis,
    schema = returns(result = artifact()),
    sink = sink_spec(
      "result",
      dir = "artifacts://production",
      format = "parquet"  # Better for production
    )
  )
```

## Working with Complex Objects

The [`lst()`](https://bbuchsbaum.github.io/parade/reference/lst.md) type
is parade’s universal container for complex R objects that don’t fit
into basic types like
[`dbl()`](https://bbuchsbaum.github.io/parade/reference/dbl.md),
[`chr()`](https://bbuchsbaum.github.io/parade/reference/chr.md), or
[`int()`](https://bbuchsbaum.github.io/parade/reference/int.md). This
includes:

- **S3 objects**: lm models, custom classes, neuroimaging objects
- **S4 objects**: Bioconductor objects, formal class structures
- **Nested structures**: Lists of lists, complex hierarchies
- **Domain objects**: Brain volumes, genomic data, spatial objects
- **Any R object**: Anything that needs to preserve its full structure

### When to use lst() vs artifact()

Choose based on size and access patterns:

| Use [`lst()`](https://bbuchsbaum.github.io/parade/reference/lst.md) (in-memory) | Use [`artifact()`](https://bbuchsbaum.github.io/parade/reference/artifact.md) (on-disk) |
|---------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------|
| Small objects (\<10MB)                                                          | Large objects (\>10MB)                                                                  |
| Frequently accessed                                                             | Accessed occasionally                                                                   |
| Complex metadata                                                                | Primary data arrays                                                                     |
| Model summaries                                                                 | Full model objects                                                                      |
| Headers/parameters                                                              | Raw data matrices                                                                       |

### Example: Neuroimaging data with lst()

``` r
# Complex neuroimaging object stays in memory
fl <- flow(subjects) |>
  stage("load_brain",
    f = function(subject_id) {
      # Returns complex S4 brain volume object
      brain <- neuroim2::read_vol(paste0(subject_id, ".nii.gz"))
      
      list(
        brain = brain,                      # Full S4 object
        volume = sum(brain@.Data > 0),     # Scalar summary
        dims = brain@space@dim              # Vector
      )
    },
    schema = returns(
      brain = lst(),    # Complex object preserved in list column
      volume = dbl(),   # Simple scalar
      dims = int()      # Integer vector
    )
  )

# The brain object maintains all S4 slots, methods, and attributes
results <- collect(fl)
first_brain <- results$brain[[1]]  # Full brain volume object
class(first_brain)  # "brain_volume" or similar S4 class
```

### Example: Mixed approach for large data

``` r
# Large data as artifact, metadata in memory
fl <- flow(subjects) |>
  stage("process_brain",
    f = function(subject_id) {
      brain <- load_and_process_brain(subject_id)
      
      list(
        # Large 3D/4D array to disk
        brain_data = brain@.Data,
        
        # Small metadata in memory
        header = brain@header,          # S4 object with metadata
        space = brain@space,            # Spatial information
        stats = summarize(brain)        # Summary tibble
      )
    },
    schema = returns(
      brain_data = artifact(),  # Large array to disk
      header = lst(),           # Complex S4 object in memory
      space = lst(),            # Spatial object in memory
      stats = tbl()             # Tibble in memory
    ),
    sink = sink_spec(
      fields = "brain_data",
      dir = "artifacts://neuroimaging",
      format = if (has_sink_format("qs2")) "qs2" else "rds"  # Fast serialization for arrays
    )
  )
```

### Key points about lst()

1.  **Preserves everything**: Class, attributes, methods, slots - all
    maintained
2.  **No type restrictions**: Any valid R object can go in
    [`lst()`](https://bbuchsbaum.github.io/parade/reference/lst.md)
3.  **List column storage**: Objects are stored in tibble list columns
4.  **Direct access**: No deserialization needed (unlike artifacts)
5.  **Memory considerations**: Keep large data as artifacts, metadata as
    [`lst()`](https://bbuchsbaum.github.io/parade/reference/lst.md)

## Flexible Type Validation (New in 0.13.0)

While [`lst()`](https://bbuchsbaum.github.io/parade/reference/lst.md)
accepts any object, you often want to validate that complex objects have
the expected class without requiring a full prototype. The flexible type
system provides lightweight validation:

### Class-based validation with isa()

Instead of creating prototype objects, use
[`isa()`](https://bbuchsbaum.github.io/parade/reference/isa.md) to
validate by class name:

``` r
# Traditional approach (requires prototype object)
schema(model = structure(list(), class = "lm"))  # Awkward!

# Flexible approach (class name only)
schema(model = isa("lm"))  # Clean and clear

# Works with any class, including S4 and package-specific
schema(
  brain = isa("neuroim2::NeuroVol"),
  model = isa(c("glm", "lm")),  # Accepts multiple classes
  network = isa("igraph")
)
```

### Optional types with maybe()

Allow fields to be NULL or match a specific type:

``` r
fl <- flow(grid) |>
  stage("fit",
    f = function(data) {
      model <- if (nrow(data) > 10) lm(y ~ x, data) else NULL
      list(
        model = model,
        n_obs = nrow(data)
      )
    },
    schema = returns(
      model = maybe(isa("lm")),  # Can be lm or NULL
      n_obs = int()
    )
  )
```

### Union types with one_of()

Accept multiple possible types:

``` r
schema(
  result = one_of(
    isa("lm"),      # Linear model
    isa("glm"),     # Generalized linear model
    isa("nls"),     # Nonlinear model
    tbl()           # Or a summary table
  )
)
```

### Predicate validation with pred()

Custom validation logic with performance hints:

``` r
# Light validation (always runs)
schema(
  data = pred(~ nrow(.) > 0, cost = "light")
)

# Heavy validation (only in full mode)
schema(
  image = pred(~ all(dim(.) == c(91, 109, 91)), cost = "full")
)

# Use with collect()
results <- collect(fl, validate = "light")  # Skip heavy checks
results_validated <- collect(fl, validate = "full")  # Run all checks
```

### Domain-specific helpers

Parade includes neuroimaging-specific validators:

``` r
# Any neuroimaging volume
schema(brain = neurovol())

# Specific volume type
schema(mask = neurovol(class = "LogicalNeuroVol"))

# With dimension validation (full mode only)
schema(mni = neurovol(dims = c(91, 109, 91)))

# Optional neuroimaging data
schema(mask = maybe_neurovol())
```

### Performance modes

Validation runs in two modes to balance safety and speed:

| Mode                | When to use        | What runs                                   |
|---------------------|--------------------|---------------------------------------------|
| `"light"` (default) | Normal execution   | Class checks, type checks, light predicates |
| `"full"`            | Testing, debugging | All checks including expensive predicates   |

``` r
# Normal execution - fast
results <- collect(flow)  # Default: validate = "light"

# Thorough validation - slower but comprehensive
results <- collect(flow, validate = "full")
```

### Combining with traditional types

Flexible types work seamlessly with parade’s standard types:

``` r
schema(
  # Standard types
  id = chr(),
  score = dbl(),
  
  # Flexible types
  model = isa("lm"),
  optimizer = maybe(isa("optim")),
  
  # Mixed in artifacts
  large_matrix = artifact(),
  metadata = one_of(lst(), tbl())
)
```

## Managing Multi-File Outputs

Many analysis pipelines, especially in neuroimaging and bioinformatics,
generate multiple output files. Parade offers three clean patterns to
handle these scenarios without tracking every file.

### Pattern A: Sentinel File (Recommended)

Track a single “done” marker file while your pipeline writes many files.
Perfect for preprocessing pipelines that generate dozens of intermediate
files.

``` r
# Sentinel pattern - track one file that signals success
sentinel_sink <- sink_quick(
  fields = "run",
  dir = "artifacts://neuro",
  template = "{.stage}/{subject}/{session}/{.row_key}",
  write = ~ {
    # Check if already done (important when overwrite = "skip")
    if (file.exists(.path)) {
      message("Skipping - already processed: ", .path)
      return(.path)  # Return sentinel path without re-running
    }
    
    out_dir <- .base  # Path without extension (becomes directory)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # 1) Run your pipeline that creates many files
    #    e.g., FSL, FreeSurfer, fMRIPrep, etc.
    # run_preprocessing_pipeline(
    #   input = .x$input_file,
    #   output_dir = out_dir,
    #   steps = c("motion_correct", "slice_timing", "normalize", "smooth")
    # )
    
    # This might create:
    # - motion_parameters.txt
    # - realigned.nii.gz
    # - mean_func.nii.gz
    # - normalized.nii.gz
    # - smoothed.nii.gz
    # - preprocessing.log
    # ... and 20+ other files
    
    # For this example, simulate creating files
    # max_motion <- 0.5  # Would come from actual preprocessing
    # temporal_snr <- 100  # Would come from actual preprocessing
    
    # 2) Write a single sentinel file as the only tracked artifact
    done_info <- list(
      ok = TRUE,
      completed = Sys.time(),
      output_dir = out_dir,
      subject = .x$subject,
      session = .x$session,
      files_created = list.files(out_dir, recursive = TRUE)
      # Add your actual QC metrics here:
      # qc_metrics = list(motion = max_motion, tsnr = temporal_snr)
    )
    jsonlite::write_json(done_info, .path, auto_unbox = TRUE, pretty = TRUE)
    .path  # Return only the sentinel path
  },
  read = ~ jsonlite::read_json(.path),
  ext = ".done.json",
  overwrite = "skip",    # Critical: skip if done file exists (resume-friendly)
  autoload = FALSE,       # Downstream typically doesn't need the sentinel
  sidecar = "json"       # Still get checksums for the sentinel
)

# Use in a flow
fl <- flow(param_grid(subject = c("s01", "s02"), session = 1:2)) |>
  stage("preprocess",
    f = function(subject, session, input_file) {
      list(run = list(
        subject = subject,
        session = session,
        input_file = input_file
      ))
    },
    schema = returns(run = artifact()),
    sink = sentinel_sink
  )

results <- collect(fl)
# Only the .done.json file is tracked, but all preprocessing outputs exist
```

**Benefits of sentinel pattern**: - **Minimal metadata**: Only one
artifact per row in manifest - **Fast resume**: `overwrite = "skip"`
skips completed rows - **Rich validation**: Sentinel can contain QC
metrics, file lists, parameters - **Clean separation**: Parade tracks
success, filesystem has all outputs

### Pattern B: Bundle Directory

Track a directory itself as the artifact when downstream stages need to
explore the contents.

``` r
bundle_sink <- sink_quick(
  fields = "bundle",
  dir = "artifacts://analysis",
  template = "{.stage}/{subject}/{session}",
  write = function(x, path) {
    # Path is the directory itself
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    
    # Run pipeline that writes into this directory
    run_analysis(
      input = x$data,
      output_dir = path,
      config = x$config
    )
    
    # Optionally write a SUCCESS marker
    writeLines("OK", file.path(path, "SUCCESS"))
    
    path  # Return the directory as the artifact
  },
  read = function(path) {
    # Custom reader can list files or check success
    list(
      files = list.files(path, recursive = TRUE),
      success = file.exists(file.path(path, "SUCCESS"))
    )
  },
  ext = ""  # No extension for directories
)
```

### Pattern C: Primary Output

Track only the main output file when there’s a clear primary result.

``` r
primary_sink <- sink_quick(
  fields = "main_result",
  dir = "artifacts://results",
  template = "{.stage}/{subject}/outputs",
  write = ~ {
    work_dir <- .base
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Pipeline writes many files
    run_full_analysis(.x, work_dir)
    # Creates: preprocessed.nii, mask.nii, stats.txt, report.pdf, etc.
    
    # But we only track the main result
    main_file <- file.path(work_dir, "final_statistics.nii.gz")
    if (!file.exists(main_file)) {
      stop("Pipeline failed to create main output")
    }
    main_file
  },
  read = ~ neuroim2::read_vol(.path),
  ext = ".nii.gz"
)
```

### Choosing the Right Pattern

| Use Case                                | Best Pattern    | Why                                  |
|-----------------------------------------|-----------------|--------------------------------------|
| Need to know success/failure + location | **A: Sentinel** | Minimal metadata, easy resume, clean |
| Downstream needs to explore outputs     | **B: Bundle**   | Directory handle for flexible access |
| Clear primary output file               | **C: Primary**  | Track only what matters              |
| Hundreds of small files                 | **A: Sentinel** | Avoid manifest bloat                 |
| QC and validation needed                | **A: Sentinel** | Embed metrics in done file           |
| Archive/backup scenario                 | **B: Bundle**   | Preserve directory structure         |

### Real-World Example: fMRI Preprocessing

``` r
# Complete fMRI preprocessing with sentinel pattern
library(parade)
paths_init()

fmri_sink <- sink_quick(
  fields = "preprocessing",
  dir = "artifacts://fmri",
  template = "{.stage}/{subject}/ses-{session}/run-{run}",
  write = ~ {
    out_dir <- .base
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Run fMRIPrep or similar
    cmd <- sprintf(
      "fmriprep %s %s participant --participant-label %s",
      .x$bids_dir, out_dir, .x$subject
    )
    system(cmd)
    
    # Write comprehensive done file
    done <- list(
      ok = file.exists(file.path(out_dir, "sub-01_space-MNI152_desc-preproc_bold.nii.gz")),
      completed = Sys.time(),
      subject = .x$subject,
      session = .x$session,
      outputs = list(
        func = Sys.glob(file.path(out_dir, "*bold*.nii.gz")),
        anat = Sys.glob(file.path(out_dir, "*T1w*.nii.gz")),
        confounds = Sys.glob(file.path(out_dir, "*confounds*.tsv")),
        reports = Sys.glob(file.path(out_dir, "*.html"))
      ),
      qc = list(
        motion = read.table(file.path(out_dir, "motion_parameters.txt")),
        tsnr = mean(read_tsnr(out_dir))
      )
    )
    
    jsonlite::write_json(done, .path, auto_unbox = TRUE, pretty = TRUE)
    .path
  },
  ext = ".prep.json",
  overwrite = "skip"  # Don't rerun completed subjects
)

# Run preprocessing pipeline
subjects_grid <- param_grid(
  subject = sprintf("sub-%02d", 1:20),
  session = 1:2,
  run = 1:4,
  bids_dir = "/data/myStudy"
)

fl <- flow(subjects_grid) |>
  stage("fmriprep",
    f = function(subject, session, run, bids_dir) {
      list(preprocessing = list(
        subject = subject,
        session = session,
        run = run,
        bids_dir = bids_dir
      ))
    },
    schema = returns(preprocessing = artifact()),
    sink = fmri_sink
  ) |>
  distribute(dist_slurm(n = 20))  # Parallel on cluster

results <- collect(fl)

# Check success and get file lists
for (i in 1:nrow(results)) {
  done <- jsonlite::read_json(results$preprocessing[[i]]$path)
  if (!done$ok) {
    warning(sprintf("Failed: %s", done$subject))
  }
}
```

### Tips for Multi-File Patterns

1.  **Always use `overwrite = "skip"`** for resume-friendly workflows
2.  **Include validation** in your sentinel (checksums, file counts, QC
    metrics)
3.  **Use `.base` in formulas** to get directory paths without
    extensions
4.  **Create row-specific directories** via template to avoid file
    collisions
5.  **Document outputs** in the sentinel for downstream discovery
6.  **Consider manifest()** later to explore the full file tree if
    needed

## Tips and best practices

1.  **Start simple**: Use
    [`sink_quick()`](https://bbuchsbaum.github.io/parade/reference/sink_quick.md)
    during development, refine with
    [`sink_spec()`](https://bbuchsbaum.github.io/parade/reference/sink_spec.md)
    for production

2.  **Choose formats wisely**:

    - RDS/QS for R-only workflows
    - Parquet for data frames and cross-language needs
    - JSON for configuration and metadata

3.  **Mix formats**: Use `formats` parameter to optimize each field
    independently

4.  **Test with sink_temp()**: Validate workflows without cluttering
    your filesystem

5.  **Register once, use everywhere**: Add custom formats to `.Rprofile`
    or package startup

6.  **Monitor with manifest()**: Track artifact growth and clean up old
    files systematically

## Summary

The format-agnostic sink system in parade 0.12.0+ provides:

- **sink_quick()** - One-liner sink creation
- **sink_temp()** - Ephemeral sinks for testing
- **Format registry** - Extensible format support
- **Per-field formats** - Optimize each artifact independently
- **Formula syntax** - Inline custom writers with minimal code
- **Built-in formats** - RDS, CSV, JSON, Parquet, and more

Whether you need quick prototyping or production-grade artifact
management, parade’s sink system scales from simple one-liners to
complex multi-format pipelines while maintaining atomic writes, metadata
tracking, and memory efficiency.
