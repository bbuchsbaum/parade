#!/usr/bin/env Rscript

library(parade)
paths_init(quiet = TRUE)

registry_dir <- 'registry://my_job'
d_path <- file.path(registry_dir, 'deferred.rds')

if (!file.exists(d_path)) {
  stop('Deferred handle not found at: ', d_path)
}

d <- readRDS(d_path)
cat('Collecting results...\n')
result <- deferred_collect(d)
cat('Collected ', nrow(result), ' rows\n', sep='')

# Save results
output_path <- file.path('/home/runner/work/parade/parade/docs/reference', 'my_job_results.rds')
saveRDS(result, output_path)
cat('Results saved to: ', output_path, '\n', sep='')
