#!/bin/bash

echo 'Cancelling jobs...'
Rscript -e "library(parade); paths_init(quiet=TRUE); d <- readRDS('registry://my_job/deferred.rds'); deferred_cancel(d, which='all')"
