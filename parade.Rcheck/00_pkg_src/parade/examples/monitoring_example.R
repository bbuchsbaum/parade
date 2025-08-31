# Parade Monitoring & Diagnostics Example
# ========================================
# This example demonstrates the enhanced monitoring features in parade v0.12.0

library(parade)

# Initialize paths for the project
paths_init(
  registry = "~/parade_jobs",
  artifacts = "~/parade_artifacts",
  data = "./data"
)

# Example 1: Explain what will be executed
# -----------------------------------------
cat("\n=== Example 1: Explaining job submission ===\n")

# Explain a function submission
explain(
  function(x, y) {
    result <- x^2 + y^2
    sqrt(result)
  },
  x = 3,
  y = 4,
  .resources = list(time = "00:30:00", cpus = 2, memory = "4G"),
  .packages = c("dplyr", "ggplot2")
)

# Explain a script submission
explain(
  "scripts/analysis.R",
  input = "data/raw.csv",
  output = "results/processed.rds",
  .resources = "gpu",  # Using a profile
  .engine = "slurm"
)

# Example 2: Dry run to preview submission
# -----------------------------------------
cat("\n=== Example 2: Dry run mode ===\n")

# Preview what would happen without actually submitting
dry_run(
  function(n) rnorm(n),
  n = 1000,
  .name = "simulation_test",
  .resources = list(time = "01:00:00", cpus = 4),
  .write_result = "results/{name}_{date}.rds"
)

# Example 3: Path macros for convenient file paths
# -------------------------------------------------
cat("\n=== Example 3: Path macro system ===\n")

# Access common directories
cat("Artifacts dir:", path$artifacts(), "\n")
cat("Data dir:", path$data(), "\n")
cat("Registry dir:", path$registry(), "\n")

# Use with subdirectories
cat("Model artifacts:", path$artifacts("models", "v2"), "\n")

# Expand path templates with macros
output_path <- path$expand(
  "results/{date}/{name}_{index}.csv",
  name = "analysis",
  index = 42
)
cat("Expanded path:", output_path, "\n")

# Use path patterns for common scenarios
timestamped <- path_patterns$timestamped("logs", "process", "log")
cat("Timestamped log:", timestamped, "\n")

experiment <- path_patterns$experiment("exp001", "model_v2", 3)
cat("Experiment path:", experiment, "\n")

# Example 4: Registry management
# -------------------------------
cat("\n=== Example 4: Registry management ===\n")

# List jobs in the registry
jobs <- registry_ls(pattern = "simulation_*", limit = 10)
print(jobs)

# Clean up old completed jobs
cat("\nCleaning up completed jobs older than 7 days...\n")
registry_clean(
  older_than = 7,
  status = "COMPLETED",
  dry_run = TRUE  # Just show what would be removed
)

# Example 5: Working with actual jobs
# ------------------------------------
cat("\n=== Example 5: Job monitoring workflow ===\n")

# Submit a few test jobs
jobs <- slurm_map(
  1:3,
  function(x) {
    Sys.sleep(x * 10)
    x^2
  },
  .name_by = glue_name("test_job_{x}"),
  .resources = list(time = "00:05:00"),
  .engine = "local"  # Use local for demo
)

# Check job status
cat("\nJob status:\n")
print(status(jobs))

# Open logs for a specific job (would open in editor)
# open_logs(jobs[[1]], which = "out")

# Example 6: Advanced path templates
# -----------------------------------
cat("\n=== Example 6: Path template builders ===\n")

# Create a reusable path template
results_path <- path_template(
  "results/{experiment}/{date}/{model}_{run}.pkl",
  experiment = "default_exp"  # Default value
)

# Use the template with different parameters
path1 <- results_path(model = "resnet", run = 1)
cat("Path 1:", path1, "\n")

path2 <- results_path(experiment = "exp002", model = "vgg", run = 5)
cat("Path 2:", path2, "\n")

# Example 7: Enhanced path macro expansion
# -----------------------------------------
cat("\n=== Example 7: Enhanced macro expansion ===\n")

# All available macros
expanded <- expand_path_macros_enhanced(
  "{user}/runs/{date}/{time}/job_{name}_{index}.out",
  name = "process",
  index = 1
)
cat("Full expansion:", expanded, "\n")

# Custom macros
custom_path <- expand_path_macros_enhanced(
  "models/{version}/{architecture}/checkpoint_{epoch}.pkl",
  version = "v2.1",
  architecture = "transformer",
  epoch = 100
)
cat("Custom macros:", custom_path, "\n")

cat("\n=== Monitoring features demonstration complete ===\n")