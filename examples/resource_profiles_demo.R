# Parade Resource Profiles Demo
# Phase 3 Feature Showcase

library(parade)

# Initialize parade paths
paths_init(quiet = TRUE)

# ===== Built-in Resource Profiles =====

# List available profiles
cat("Built-in profiles:\n")
print(profile_list())

# Show details
cat("\nProfile details:\n")
print(profile_list(details = TRUE))

# ===== Using Profiles with Jobs =====

# Use built-in profile by name
cat("\n=== Using built-in profiles ===\n")

# Quick test job
job1 <- slurm_call(
  function() {
    Sys.info()["nodename"]
  },
  resources = "test",  # 30min, 4GB, 2 CPUs
  engine = "local"
)

# GPU job
job2 <- slurm_call(
  function() {
    # Simulate GPU computation
    matrix(runif(1000000), 1000, 1000) %*% matrix(runif(1000000), 1000, 1000)
    "GPU computation complete"
  },
  resources = "gpu",  # 12h, 32GB, 8 CPUs, 1 GPU
  engine = "local"
)

# ===== Creating Custom Profiles =====

cat("\n=== Creating custom profiles ===\n")

# Fluent interface with chaining
my_profile <- profile() %>%
  time("6:00:00") %>%
  mem("24G") %>%
  cpus(12) %>%
  partition("compute")

print(my_profile)

# Use custom profile
job3 <- slurm_call(
  function(n) {
    sum(1:n)
  },
  n = 1000000,
  resources = my_profile,
  engine = "local"
)

# ===== Profile Inheritance =====

cat("\n=== Profile inheritance ===\n")

# Start with base profile
base_compute <- profile("base_compute") %>%
  time("2:00:00") %>%
  mem("8G") %>%
  cpus(4) %>%
  partition("standard")

# Extend for specific needs
extended <- profile(base = base_compute) %>%
  time("8:00:00") %>%  # Override time
  mem("32G")           # Override memory
# CPUs and partition are inherited

print(extended)

# ===== Registering Custom Profiles =====

cat("\n=== Registering profiles ===\n")

# Register for machine learning tasks
profile_register("ml_train",
  profile() %>%
    time("24:00:00") %>%
    mem("128G") %>%
    cpus(32) %>%
    gpus(4, type = "v100") %>%
    partition("gpu")
)

# Register for data processing
profile_register("data_proc",
  profile() %>%
    time("4:00:00") %>%
    mem("64G") %>%
    cpus(16) %>%
    partition("highmem")
)

# Register minimal profile for debugging
profile_register("debug",
  profile() %>%
    time("0:10:00") %>%
    mem("2G") %>%
    cpus(1)
)

# List all profiles including custom ones
cat("\nAll registered profiles:\n")
print(profile_list(details = TRUE))

# ===== Using with slurm_map =====

cat("\n=== Using profiles with slurm_map ===\n")

# Process multiple datasets with consistent resources
datasets <- 1:3

jobs <- slurm_map(
  datasets,
  function(id) {
    # Simulate data processing
    data <- matrix(rnorm(1000 * id), ncol = 10)
    list(
      id = id,
      mean = mean(data),
      sd = sd(data)
    )
  },
  .resources = "data_proc",  # Use registered profile
  .name_by = index("dataset"),
  .engine = "local"
)

# Collect results
results <- collect(jobs)
cat("\nProcessing results:\n")
print(results)

# ===== Legacy String Shortcuts =====

cat("\n=== Legacy shortcuts still work ===\n")

# These shortcuts are still supported
job_cpu <- slurm_call(function() "cpu job", 
                      resources = "cpu16",  # 16 CPUs
                      engine = "local")

job_mem <- slurm_call(function() "memory job",
                      resources = "mem64G",  # 64GB memory  
                      engine = "local")

# ===== Profile Management =====

cat("\n=== Profile management ===\n")

# Get a specific profile
ml_profile <- profile_get("ml_train")
cat("\nML training profile:\n")
print(ml_profile)

# Remove a profile
profile_remove("debug")
cat("\nAfter removing 'debug':\n")
print(profile_list())

# Clear all custom profiles (keep defaults)
# profile_clear()
# profile_init_defaults()  # Restore defaults

# ===== Advanced: Dynamic Profile Selection =====

cat("\n=== Dynamic profile selection ===\n")

# Function to select profile based on data size
get_profile_for_size <- function(size) {
  if (size < 1000) {
    "test"
  } else if (size < 10000) {
    "standard"
  } else if (size < 100000) {
    "highmem"
  } else {
    "long"
  }
}

# Process with dynamic resource allocation
data_sizes <- c(500, 5000, 50000)

dynamic_jobs <- slurm_map(
  data_sizes,
  function(size) {
    # Process based on size
    sample(1:size, min(size, 100))
  },
  .resources = sapply(data_sizes, get_profile_for_size),
  .name_by = glue_name("size-{.x}"),
  .engine = "local"
)

# ===== Summary =====

cat("\n=== Resource Profiles Summary ===\n")
cat("✅ Fluent interface with chaining (time, mem, cpus, gpus)\n")
cat("✅ Built-in profiles (test, standard, highmem, gpu, long)\n")
cat("✅ Custom profile registration and management\n")
cat("✅ Profile inheritance for extending base configs\n")
cat("✅ Integration with slurm_call() and slurm_map()\n")
cat("✅ Legacy string shortcuts still supported\n")
cat("✅ Dynamic profile selection based on context\n")

cat("\nNext: Phase 4 - Flow Control (waves, error handling, grids)\n")