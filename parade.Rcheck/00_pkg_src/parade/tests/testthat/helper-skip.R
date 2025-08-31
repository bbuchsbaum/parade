# Helper function to skip tests when SLURM is not available

skip_if_no_slurm <- function() {
  if (Sys.which("squeue") == "") {
    testthat::skip("SLURM not available on this system")
  }
}

# Helper function to check if running in CI environment
skip_on_ci_without_slurm <- function() {
  if (isTRUE(as.logical(Sys.getenv("CI", "FALSE")))) {
    skip_if_no_slurm()
  }
}

# Load the new parser functions if available
if (file.exists(system.file("R", "slurm_parsers.R", package = "parade"))) {
  source(system.file("R", "slurm_parsers.R", package = "parade"))
}