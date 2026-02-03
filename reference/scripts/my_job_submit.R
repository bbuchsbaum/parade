#!/usr/bin/env Rscript

library(parade)
paths_init(quiet = TRUE)

# Load the deferred handle
registry_dir <- 'registry://my_job'
d_path <- file.path(registry_dir, 'deferred.rds')

if (!file.exists(d_path)) {
  stop('Deferred handle not found at: ', d_path)
}

d <- readRDS(d_path)
cat('Loaded deferred handle\n')
cat('Backend:   ', d$backend, '\n', sep='')
cat('Registry:  ', d$registry_dir, '\n', sep='')
cat('Index dir: ', resolve_path(d$index_dir), '\n', sep='')

