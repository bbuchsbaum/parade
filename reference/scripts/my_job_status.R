#!/usr/bin/env Rscript

library(parade)
paths_init(quiet = TRUE)

registry_dir <- 'registry://my_job'
d_path <- file.path(registry_dir, 'deferred.rds')

if (!file.exists(d_path)) {
  stop('Deferred handle not found at: ', d_path)
}

d <- readRDS(d_path)
status <- deferred_status(d, detail = TRUE)
print(status)
