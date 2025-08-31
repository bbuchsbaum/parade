# Parade Unified API Demo
# Phase 1 & 2 Features Showcase

library(parade)

# Initialize parade paths
paths_init(quiet = TRUE)

# ===== Basic Function Submission with Local Engine =====

# Simple function call with local execution
job <- slurm_call(
  function(x, y) x^2 + y^2,
  x = 3,
  y = 4,
  name_by = "index",
  engine = "local"
)

print(job)
# Result is immediately available
cat("Result:", job$result, "\n\n")

# ===== Map Functions with Formula Notation =====

# Process multiple files with formula notation
files <- c("data1.csv", "data2.csv", "data3.csv")

jobs <- slurm_map(
  files,
  ~ paste("Processing", .x),
  .name_by = "stem",
  .engine = "local"
)

print(jobs)
results <- collect(jobs)
print(results)

# ===== Advanced Name Helpers =====

# Using stem with pattern extraction
sample_files <- c("sample_001_raw.txt", "sample_002_raw.txt", "sample_003_raw.txt")

jobs <- slurm_map(
  sample_files,
  ~ tools::file_path_sans_ext(.x),
  .name_by = stem("sample_(\\d+)"),
  .engine = "local"
)

# Check generated names
cat("\nGenerated job names:\n")
sapply(jobs, function(j) j$name)

# ===== Parallel Mapping with pmap =====

# Multiple arguments in parallel
df <- data.frame(
  x = 1:4,
  y = 5:8,
  method = c("add", "multiply", "add", "multiply")
)

jobs <- slurm_pmap(
  df,
  function(x, y, method) {
    if (method == "add") x + y else x * y
  },
  .name_by = glue_name("{method}-{x}-{y}"),
  .engine = "local"
)

print(jobs)
results <- collect(jobs)
cat("\nResults:", results, "\n")

# ===== Path Macros in write_result =====

temp_dir <- tempdir()

jobs <- slurm_map(
  1:3,
  ~ .x^3,
  .name_by = index("cube"),
  .write_result = file.path(temp_dir, "results_{name}_{index}.rds"),
  .engine = "local"
)

# Files are saved with expanded macros
saved_files <- list.files(temp_dir, pattern = "results_cube.*\\.rds", full.names = TRUE)
cat("\nSaved files:\n")
print(saved_files)

# ===== Jobset Operations =====

# Create a larger jobset
big_jobs <- slurm_map(
  1:10,
  ~ {
    Sys.sleep(0.1)  # Simulate work
    .x * pi
  },
  .name_by = index("calc", width = 2),
  .engine = "local"
)

# Status overview
status_df <- status(big_jobs)
print(status_df)

# Subsetting
first_five <- big_jobs[1:5]
cat("\nFirst five jobs:\n")
print(first_five)

# Filtering (all are completed in local mode)
done <- completed(big_jobs)
cat("\nCompleted jobs:", length(done), "\n")

# Progress convenience: works for both slurm_map() jobsets and one‑off jobs
# Wrap a single function submission as a jobset for the same verb surface
single <- slurm_call(
  function(x) { Sys.sleep(0.2); x * 2 },
  x = 21,
  .as_jobset = TRUE,
  engine = "local"
)
single |> progress() |> collect()

# ===== Argument Helpers =====

# CLI arguments for scripts
cli_args <- args_cli(
  input = "data.csv",
  output = "results.rds",
  verbose = TRUE,
  threads = 4
)
cat("\nCLI arguments:\n")
print(cli_args)

# Function call arguments
call_args <- args_call(
  data = mtcars,
  formula = mpg ~ cyl + wt,
  method = "lm"
)
cat("\nFunction arguments:\n")
str(call_args)

# Auto-detection
auto1 <- args(file = "input.txt", verbose = TRUE)  # Looks like CLI
auto2 <- args(data = iris, n = 100)  # Looks like function call

cat("\nAuto-detected as CLI:", class(auto1), "\n")
cat("Auto-detected as list:", class(auto2), "\n")

# ===== Summary =====

cat("\n=== Parade Unified API Summary ===\n")
cat("✅ Enhanced slurm_call() with name_by and local engine\n")
cat("✅ slurm_map() for functional programming patterns\n")
cat("✅ slurm_pmap() for parallel argument mapping\n")
cat("✅ Formula notation support (~)\n")
cat("✅ Rich naming helpers (stem, index, digest, glue_name)\n")
cat("✅ Path macro expansion in write_result\n")
cat("✅ Unified job class hierarchy\n")
cat("✅ Complete jobset operations (await, collect, status, etc.)\n")
cat("✅ Argument helpers for scripts and functions\n")

cat("\nNext: Phase 3 - Resource profiles and management\n")
