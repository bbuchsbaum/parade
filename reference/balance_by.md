# Balance work across groups

Distributes items across groups to balance workload. Useful for ensuring
even distribution of tasks across workers.

## Usage

``` r
balance_by(x, n_groups, weights = NULL)
```

## Arguments

- x:

  Vector or data frame to balance

- n_groups:

  Number of groups to create

- weights:

  Optional weights for each item (higher = more work)

## Value

List of balanced groups

## Examples

``` r
# \donttest{
# Balance items across 3 groups
groups <- balance_by(1:10, n_groups = 3)

# Balance with weights (e.g., file sizes)
files <- c("small.csv", "medium.csv", "large.csv")
sizes <- c(100, 500, 1000)
groups <- balance_by(files, n_groups = 2, weights = sizes)
# }
```
