## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(parade)
# 
# # Initialize mirai with auto-detected cores
# mirai_init()
# 
# # Or use the distribution interface
# fl <- flow(grid) |>
#   stage("process", function(x) x^2, schema = returns(y = dbl())) |>
#   distribute(use_mirai_local()) |>
#   collect()

## -----------------------------------------------------------------------------
# # Basic local setup
# dist <- dist_mirai(n = 8)
# 
# # With explicit configuration
# dist <- dist_mirai(
#   n = parallel::detectCores(),
#   dispatcher = TRUE,  # Enable load balancing
#   within = "mirai",   # Use mirai for nested parallelization
#   workers_within = 4  # Nested workers per daemon
# )
# 
# # Apply to flow
# fl |> distribute(dist) |> collect()

## -----------------------------------------------------------------------------
# # Configure SLURM-launched daemons
# dist <- use_mirai_slurm(
#   n = 16,                    # Number of daemon jobs
#   partition = "compute",     # SLURM partition
#   time = "2:00:00",         # Wall time
#   mem = "32G",              # Memory per daemon
#   cpus = 8,                 # CPUs per daemon
#   tls = TRUE,               # Use TLS encryption
#   port = 5555               # TLS port
# )
# 
# # Submit flow with SLURM daemons
# fl |>
#   distribute(dist) |>
#   submit()  # Returns immediately
# 
# # Monitor status
# deferred_status(handle)
# results <- deferred_collect(handle)

## -----------------------------------------------------------------------------
# # SSH with tunneling (for firewalled nodes)
# dist <- use_mirai_ssh(
#   remotes = c("ssh://node1", "ssh://node2", "ssh://node3"),
#   tunnel = TRUE,      # Use SSH tunneling
#   port = 40491       # Local port for tunnel
# )
# 
# # Direct SSH (when ports are open)
# dist <- use_mirai_ssh(
#   remotes = c("ssh://compute1", "ssh://compute2"),
#   tunnel = FALSE
# )
# 
# fl |> distribute(dist) |> collect()

## -----------------------------------------------------------------------------
# library(mirai)
# library(future)
# 
# # Set up custom TLS-secured daemons
# url <- host_url(tls = TRUE, port = 5555)
# 
# # Configure SLURM submission
# opts <- paste(
#   "#SBATCH --partition=highmem",
#   "#SBATCH --time=04:00:00",
#   "#SBATCH --mem=64G",
#   "#SBATCH --job-name=my-analysis",
#   sep = "\n"
# )
# 
# cfg <- cluster_config(command = "sbatch", options = opts)
# 
# # Launch daemons
# daemons(n = 32, url = url, remote = cfg, dispatcher = TRUE)
# 
# # Use with parade via future
# plan(future.mirai::mirai_cluster)
# fl |> collect()

## -----------------------------------------------------------------------------
# # Start with a few daemons
# mirai_init(n = 4)
# 
# # Scale up for intensive processing
# mirai_scale(16)
# results <- collect(heavy_flow)
# 
# # Scale down when done
# mirai_scale(4)
# 
# # Check status
# mirai_status()
# mirai_dispatcher_status()

## -----------------------------------------------------------------------------
# # Fine-grained parallelism (many small tasks)
# dist_mirai(
#   n = 16,
#   dispatcher = TRUE,     # Essential for load balancing
#   chunks_per_job = 1     # One group per task
# )
# 
# # Coarse-grained parallelism (fewer large tasks)
# dist_mirai(
#   n = 8,
#   dispatcher = FALSE,    # Direct task assignment
#   chunks_per_job = 10    # Bundle groups together
# )

## -----------------------------------------------------------------------------
# library(parade)
# 
# # Parameter grid with 10,000 combinations
# grid <- expand.grid(
#   alpha = seq(0.1, 1, by = 0.1),
#   beta = seq(0.5, 5, by = 0.5),
#   n_sim = 1:100
# )
# 
# # Use mirai to bypass connection limits
# fl <- flow(grid) |>
#   stage("simulate", function(alpha, beta, n_sim) {
#     set.seed(n_sim)
#     data <- rgamma(1000, shape = alpha, rate = beta)
#     list(
#       mean = mean(data),
#       var = var(data),
#       ks_p = ks.test(data, "pgamma", alpha, beta)$p.value
#     )
#   }, schema = returns(mean = dbl(), var = dbl(), ks_p = dbl())) |>
#   distribute(dist_mirai(n = 200))  # 200 workers!
# 
# results <- collect(fl)

## -----------------------------------------------------------------------------
# # Development: test locally
# fl_dev <- fl |>
#   distribute(use_mirai_local(n = 4))
# 
# test_results <- fl_dev |> collect(limit = 100)
# 
# # Production: scale to cluster
# fl_prod <- fl |>
#   distribute(use_mirai_slurm(
#     n = 64,
#     partition = "compute",
#     time = "12:00:00",
#     mem = "256G",
#     account = "project-123"
#   ))
# 
# handle <- submit(fl_prod)
# 
# # Monitor progress
# while (!all(deferred_status(handle)$done)) {
#   status <- deferred_status(handle)
#   print(status)
#   Sys.sleep(30)
# }
# 
# final_results <- deferred_collect(handle)

## -----------------------------------------------------------------------------
# # Detect environment and choose appropriate backend
# get_distribution <- function() {
#   if (Sys.getenv("SLURM_JOB_ID") != "") {
#     # Running on SLURM
#     use_mirai_slurm(n = 32, partition = "compute")
#   } else if (file.exists("~/.ssh/compute_nodes")) {
#     # Has SSH access to compute nodes
#     nodes <- readLines("~/.ssh/compute_nodes")
#     use_mirai_ssh(remotes = nodes, tunnel = TRUE)
#   } else {
#     # Local development
#     use_mirai_local()
#   }
# }
# 
# # Portable workflow
# fl |>
#   distribute(get_distribution()) |>
#   collect()

## -----------------------------------------------------------------------------
# # For homogeneous tasks (similar runtime)
# dist_mirai(n = 16, dispatcher = FALSE)
# 
# # For heterogeneous tasks (varying runtime)
# dist_mirai(n = 16, dispatcher = TRUE)
# 
# # For embarrassingly parallel workflows
# dist_mirai(
#   n = 32,
#   dispatcher = TRUE,
#   chunks_per_job = 1  # Maximum parallelism
# )

## -----------------------------------------------------------------------------
# # Check daemon status
# mirai_status()
# 
# # Restart daemons
# mirai_stop()
# mirai_init()

## -----------------------------------------------------------------------------
# # Ensure dispatcher is enabled
# dist_mirai(n = 16, dispatcher = TRUE)
# 
# # Check dispatcher status
# mirai_dispatcher_status()

## -----------------------------------------------------------------------------
# # Test SSH manually first
# system("ssh node1 echo 'connected'")
# 
# # Use verbose mode for debugging
# Sys.setenv(MIRAI_DEBUG = "TRUE")

## -----------------------------------------------------------------------------
# # Check SLURM submission manually
# mirai::daemons(
#   n = 1,
#   url = mirai::host_url(tls = TRUE),
#   remote = mirai::cluster_config(
#     command = "sbatch",
#     options = "#SBATCH --partition=debug\n#SBATCH --time=00:05:00"
#   )
# )
# 
# # Check SLURM queue
# system("squeue -u $USER")

## -----------------------------------------------------------------------------
# dist_mirai(n = 16, tls = TRUE, port = 5555)

## -----------------------------------------------------------------------------
# # During execution
# mirai_dispatcher_status()
# 
# # After completion
# mirai_status()

## -----------------------------------------------------------------------------
# # Development
# fl |> distribute(use_mirai_local(n = 2)) |> collect(limit = 10)
# 
# # Production
# fl |> distribute(use_mirai_slurm(n = 64)) |> submit()

