# Parse squeue output

Parse squeue output

## Usage

``` r
.parse_squeue_output(output)
```

## Arguments

- output:

  Character vector of squeue command output

## Value

List with job state information (state, time, timelimit, cpus, nodes,
reason, nodelist)
