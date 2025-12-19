# Create a Beowulf cluster for parallel computing

Creates a Beowulf cluster configuration from machine IPs, core counts,
and user credentials.

## Usage

``` r
beowulf_cluster(
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000",
  outfile = NULL
)
```

## Arguments

- cluster.ips:

  Character vector of machine IP addresses in the cluster. The first IP
  is the main node (typically the machine running this code). Default:
  `NULL`.

- cluster.cores:

  Integer vector of core counts for each machine. Must match the length
  of `cluster.ips`. Default: `NULL`.

- cluster.user:

  Character string for the user name across all machines. Default:
  current system user.

- cluster.port:

  Character string specifying the communication port. Default:
  `"11000"`.

- outfile:

  Character string or `NULL`. Path to append worker messages, `""` to
  print to console, or `NULL` (default) for `/dev/null` (Linux) or
  `nul:` (Windows).

## Value

Cluster object created by
[`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html),
ready for registration with
[`doParallel::registerDoParallel()`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html).

## Details

**Network requirements**: Firewalls on all machines must allow traffic
on the specified port.

**Usage workflow**:

1.  Create cluster with this function

2.  Register with
    [`doParallel::registerDoParallel()`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html)

3.  Run parallelized code (e.g., foreach loops)

4.  Stop cluster with
    [`parallel::stopCluster()`](https://rdrr.io/r/parallel/makeCluster.html)

## See also

Other utilities:
[`.vif_to_df()`](https://blasbenito.github.io/spatialRF/reference/dot-vif_to_df.md),
[`auc()`](https://blasbenito.github.io/spatialRF/reference/auc.md),
[`objects_size()`](https://blasbenito.github.io/spatialRF/reference/objects_size.md),
[`optimization_function()`](https://blasbenito.github.io/spatialRF/reference/optimization_function.md),
[`prepare_importance_spatial()`](https://blasbenito.github.io/spatialRF/reference/prepare_importance_spatial.md),
[`rescale_vector()`](https://blasbenito.github.io/spatialRF/reference/rescale_vector.md),
[`root_mean_squared_error()`](https://blasbenito.github.io/spatialRF/reference/root_mean_squared_error.md),
[`setup_parallel_execution()`](https://blasbenito.github.io/spatialRF/reference/setup_parallel_execution.md),
[`standard_error()`](https://blasbenito.github.io/spatialRF/reference/standard_error.md),
[`statistical_mode()`](https://blasbenito.github.io/spatialRF/reference/statistical_mode.md),
[`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md),
[`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create cluster with 3 machines
beowulf.cluster <- beowulf_cluster(
  cluster.ips = c(
    "192.168.1.10",  # main node
    "192.168.1.11",
    "192.168.1.12"
  ),
  cluster.cores = c(7, 4, 4),
  cluster.user = "username",
  cluster.port = "11000"
)

# Register cluster for parallel processing
doParallel::registerDoParallel(cl = beowulf.cluster)

# Run parallelized code (e.g., foreach loop)
# your_parallel_code_here

# Stop cluster when done
parallel::stopCluster(cl = beowulf.cluster)
} # }

```
