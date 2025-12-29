# Migration Guide: future and future.apply Packages

**Personal Reference Document**
Created: 2025-12-29
Purpose: Comprehensive guide for migrating spatialRF from foreach+doParallel to future+future.apply

---

## ⚠️ CRITICAL PRINCIPLES

### 1. Never Call plan() in Package Functions

**THE GOLDEN RULE OF FUTURE:**

> **Package functions must NEVER call `future::plan()`**

This is the user's responsibility. Package functions should:
- ✅ **Query** the current plan using `nbrOfWorkers()` and `plan()`
- ✅ **Respect** whatever plan the user has set
- ✅ **Adapt** internal behavior based on detected parallelization
- ❌ **NEVER** call `plan()` to change the user's configuration

### 2. Use future::availableCores() Instead of parallel::detectCores()

**Replace `parallel::detectCores()` with `future::availableCores()`**

The `future` package re-exports `availableCores()` from `parallelly`, which is superior to `parallel::detectCores()`:

**Why availableCores() is better:**
- ✅ **Never returns NA** - always returns at least 1
- ✅ **Respects resource limits** - honors cgroups, containers, HPC schedulers (Slurm, PBS, SGE)
- ✅ **Prevents oversubscription** - detects actual available cores, not total system cores
- ✅ **Safe omit parameter** - `availableCores(omit = 1)` never returns 0
- ✅ **Avoids connection limits** - prevents exceeding R's 125 connection limit on high-core machines

**Why detectCores() should be avoided:**
- ❌ Can return `NA` (causes downstream errors)
- ❌ Returns total cores regardless of actual allocation (problems in HPC, containers)
- ❌ `detectCores() - 1` can return 0 or negative on single-core systems
- ❌ Exceeds R connection limits on 192+ core machines

**Usage:**
```r
# OLD (parallel)
n.cores <- parallel::detectCores() - 1

# NEW (future - cleaner, re-exported from parallelly)
n.cores <- future::availableCores(omit = 1)  # Safer, respects limits
```

This allows removing `parallel` as a dependency - just use `future::availableCores()`.

### Why This Matters

1. **User control:** Parallelization strategy is a user-level decision
2. **Composability:** Multiple packages can work together without conflicts
3. **Flexibility:** Users can switch backends (local, cluster, HPC) without code changes
4. **Predictability:** Functions don't have surprising side effects

### What This Means for spatialRF

**CORRECT approach:**
```r
rf_evaluate <- function(model, ..., n.cores = NULL) {
  # 1. Detect user's plan
  plan_workers <- nbrOfWorkers()

  # 2. Adapt ranger threading based on detected plan
  if (plan_workers > 1) {
    # User set parallel plan - disable ranger threading
    ranger.arguments$num.threads <- 1
  } else {
    # Sequential plan - use n.cores for ranger
    ranger.arguments$num.threads <- n.cores %||% future::availableCores(omit = 1)
  }

  # 3. Use future.apply (respects user's plan)
  result <- future_lapply(folds, function(fold) {
    rf(..., ranger.arguments = ranger.arguments)
  })
}

# User controls parallelization:
plan(multisession, workers = 8)  # User's choice
result <- rf_evaluate(model)     # Function respects it
```

**WRONG approach (DO NOT DO THIS):**
```r
rf_evaluate <- function(model, ..., n.cores = 8) {
  # ❌ NEVER DO THIS - overrides user's plan!
  plan(multisession, workers = n.cores)

  result <- future_lapply(...)
}
```

### The n.cores Parameter Strategy

Since we cannot call `plan()`, `n.cores` should **only control ranger's threading:**

```r
# n.cores controls ranger::ranger(num.threads), NOT R-level parallelization
rf_evaluate <- function(model, n.cores = NULL) {
  plan_workers <- nbrOfWorkers()

  # Default n.cores based on context
  if (is.null(n.cores)) {
    n.cores <- if (plan_workers > 1) 1 else parallel::detectCores() - 1
  }

  # Warn about oversubscription
  if (plan_workers > 1 && n.cores > 1) {
    warning(
      "Parallel plan detected (", plan_workers, " workers) with n.cores = ", n.cores, ". ",
      "This creates ", plan_workers * n.cores, " total threads. ",
      "Consider n.cores = 1 to avoid oversubscription.",
      call. = FALSE
    )
  }

  ranger.arguments$num.threads <- n.cores

  # future_lapply respects user's plan
  future_lapply(...)
}
```

**User decides parallelization strategy:**
- `plan(sequential)` + `n.cores = 8` → ranger threading (8 threads total)
- `plan(multisession, 8)` + `n.cores = 1` → R-level parallel (8 workers × 1 thread)
- `plan(multisession, 8)` + default n.cores → auto-detects, sets n.cores = 1

---

## Table of Contents

1. [spatialRF's Two Parallelization Patterns](#spatialrfs-two-parallelization-patterns)
2. [Core Concepts](#core-concepts)
3. [The future Package](#the-future-package)
4. [The future.apply Package](#the-futureapply-package)
5. [Integration with progressr](#integration-with-progressr)
6. [Migration Patterns](#migration-patterns)
7. [Best Practices & Pitfalls](#best-practices--pitfalls)
8. [Comparison: foreach vs future.apply](#comparison-foreach-vs-futureapply)

---

## spatialRF's Two Parallelization Patterns

### Overview

spatialRF uses **two independent parallelization mechanisms** that must be carefully coordinated:

1. **ranger's C++ multithreading** - Internal tree-building parallelization
2. **R-level multiprocessing** - Cross-validation, tuning, and iteration parallelization via foreach+doParallel

These operate at different levels and use different concurrency models. Understanding their interaction is critical for efficient resource utilization.

---

### Pattern 1: ranger's C++ Multithreading

#### Implementation

ranger implements **native multithreading** in C++ to parallelize tree construction within a single random forest model.

**Control parameter:** `num.threads` in `ranger::ranger()`

**In spatialRF:**
- `rf()` exposes this as `n.cores` (line 139: `num.threads <- n.cores`)
- Default (current): `parallel::detectCores() - 1`
- Default (future): `future::availableCores(omit = 1)`
- Passed directly to ranger as `num.threads` argument

#### Threading Model

From ranger source code:
- Uses C++ native threads (not R processes)
- Default: 2 threads if not specified
- Priority order for configuration:
  1. `num.threads` function argument
  2. `R_RANGER_NUM_THREADS` environment variable
  3. `options(ranger.num.threads = N)`
  4. `options(Ncpus = N)`
  5. Default: 2

#### Critical Constraint: Regularization

When regularization is used (`regularization.factor` ≠ all 1s), ranger **forces single-threaded execution**:
- Sets `num.threads <- 1` internally
- Reason: All trees need shared access to variable inclusion tracking
- Cannot be overridden

#### When Active

This parallelization is **always active** when:
- Calling `rf()` directly
- `n.cores > 1` (or `num.threads > 1` via `ranger.arguments`)
- No regularization in use

---

### Pattern 2: R-Level Multiprocessing via foreach

#### Implementation

Multiple functions use foreach+doParallel to parallelize **across iterations**:
- `rf_evaluate()`: Parallelize across spatial folds (R/rf_evaluate.R:221)
- `rf_tuning()`: Parallelize across hyperparameter combinations
- `rf_repeat()`: Parallelize across model repetitions
- `rank_spatial_predictors()`: Parallelize across predictor ranking iterations
- `select_spatial_predictors_sequential()`: Parallelize across candidate selections
- `the_feature_engineer()`: Parallelize across interaction testing
- `make_spatial_folds()`: Parallelize fold generation

#### Cluster Management Pattern

**Current implementation** uses `setup_parallel_execution()` (R/setup_parallel_execution.R):

```r
parallel_config <- setup_parallel_execution(cluster, n.cores)
on.exit(parallel_config$cleanup(), add = TRUE)
```

**Four execution modes:**
1. **user_backend**: User pre-registered backend (via `registerDoParallel()` etc.) - no interference
2. **external_cluster**: User provides cluster object - function registers but doesn't stop it
3. **internal_cluster**: Function creates and manages cluster (n.cores > 1)
4. **sequential**: Single-core execution (n.cores = 1)

**Key logic:**
- Detects existing backends via `foreach::getDoParRegistered()`
- Distinguishes user backends from doSEQ (default sequential backend)
- Only stops clusters it creates (`internal_cluster` mode)
- External clusters passed by user are never stopped

#### Typical foreach Pattern

From `rf_evaluate()` (R/rf_evaluate.R:221-248):

```r
evaluation.df <- foreach::foreach(
  i = seq(1, length(spatial.folds), by = 1),
  .combine = "rbind",
  .verbose = FALSE
) %dopar% {
  # Fit model on training fold
  m.training <- spatialRF::rf(
    data = data.training,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    ranger.arguments = ranger.arguments.training,
    seed = seed,
    n.cores = 1,              # KEY: Disable ranger threading
    cluster = NULL,
    verbose = FALSE
  )

  # Predict on testing fold
  predicted <- stats::predict(
    object = m.training,
    data = data.testing,
    type = "response",
    num.threads = 1           # KEY: Explicit threading control
  )$predictions

  # Compute metrics...
}
```

---

### Critical Coordination: Preventing Resource Oversubscription

#### The Problem

Running both parallelization patterns simultaneously causes **severe resource contention**:
- R-level: 8 parallel workers
- ranger-level: 8 threads per worker
- **Total: 64 concurrent threads** competing for 8 physical cores
- Result: Context switching overhead, memory thrashing, slower execution

#### The Solution

**When using R-level parallelization, disable ranger's threading:**

```r
# In rf_evaluate.R line 92-94:
if (repetitions > 1) {
  ranger.arguments$num.threads <- 1
}

# In the foreach loop line 245:
m.training <- spatialRF::rf(
  ...,
  n.cores = 1,              # Explicit disable
  ...
)

# In predict call line 255:
predicted <- stats::predict(
  ...,
  num.threads = 1           # Also disable for prediction
)
```

**Principle:** Parallelize at the **outer loop level** (R-level), keep inner operations (ranger) sequential.

#### Performance Implications

**Scenario 1: Single large model**
- **Best:** Use ranger's multithreading (`n.cores = 8`, no foreach)
- Why: Low overhead, shared memory, efficient tree parallelization

**Scenario 2: Multiple models (CV, tuning, repetitions)**
- **Best:** R-level parallelization (foreach with 8 workers, `num.threads = 1`)
- Why: Parallelizes across independent model fits, no coordination overhead

**Scenario 3: Single model with repetitions**
- **Best:** Depends on ratio - usually R-level for 8+ repetitions

---

### Migration Impact: future vs foreach

The two-pattern structure **remains the same** with future:

#### Pattern 1: ranger Threading (No Change)

```r
# BEFORE and AFTER - identical
m <- rf(
  data = data,
  dependent.variable.name = response,
  predictor.variable.names = predictors,
  n.cores = 8  # Still controls ranger's num.threads
)
```

ranger's C++ threading is **orthogonal** to future - it works identically.

#### Pattern 2: R-Level Parallelization (Changes)

**BEFORE (foreach+doParallel):**

```r
# Setup
parallel_config <- setup_parallel_execution(cluster, n.cores)
on.exit(parallel_config$cleanup(), add = TRUE)

# Execute
result <- foreach(i = seq_along(folds), .combine = "rbind") %dopar% {
  spatialRF::rf(..., n.cores = 1)  # Disable ranger threading
}
```

**AFTER (future.apply):**

```r
# Setup
plan(multisession, workers = n.cores)

# Execute
result <- do.call(rbind, future_lapply(seq_along(folds), function(i) {
  spatialRF::rf(..., n.cores = 1)  # Still disable ranger threading
}))
```

**Key insight:** Coordination requirement stays the same - when parallelizing at R-level, set `n.cores = 1` for ranger.

---

### Function-Level Behavior Summary

| Function | Uses foreach? | Sets ranger num.threads? | Behavior |
|----------|---------------|--------------------------|----------|
| `rf()` | No | Via `n.cores` param | ranger threads = n.cores (default: detectCores()-1) |
| `rf_spatial()` | No (calls rf()) | Inherits from rf() | Same as rf() |
| `rf_evaluate()` | Yes | Force 1 if repetitions > 1 | R-parallel across folds, ranger sequential |
| `rf_tuning()` | Yes | Force 1 (implicit) | R-parallel across hyperparams, ranger sequential |
| `rf_repeat()` | Yes | Force 1 (implicit) | R-parallel across repetitions, ranger sequential |
| `rank_spatial_predictors()` | Yes | Force 1 (implicit) | R-parallel across rankings |
| `select_spatial_predictors_*()` | Yes | Force 1 (implicit) | R-parallel across selections |
| `the_feature_engineer()` | Yes | Force 1 (implicit) | R-parallel across interactions |
| `make_spatial_folds()` | Yes | N/A | R-parallel fold generation |

**Pattern:** Functions that parallelize multiple model fits force `num.threads = 1`.

---

### Recommendations for Future Migration

1. **Preserve the two-pattern structure** - don't try to unify them
2. **Keep coordination logic** - when using future.apply, still set `n.cores = 1` for ranger
3. **Let users control top-level parallelization:**
   - Via `plan()` for R-level (future.apply)
   - Via `n.cores` for ranger-level (single models)
4. **Consider removing `n.cores` parameter eventually:**
   - Rely on `plan()` for R-level parallelization
   - Expose ranger's `num.threads` directly via `ranger.arguments`
   - Simplifies API and leverages future's superior backend abstraction

---

### Critical Conflict: User Sets Both plan() and n.cores

#### The Scenario

**User code:**
```r
library(future)
plan(multisession, workers = 8)  # R-level parallelization

rf_repeat(
  model = m,
  repetitions = 10,
  n.cores = 8  # What does this mean now?
)
```

**The conflict:** What should `n.cores = 8` control?
- R-level parallelization across repetitions?
- ranger's C++ threading within each repetition?
- Both? (Would cause 8 × 8 = 64 threads - oversubscription!)

#### Current Protection (foreach implementation)

`rf_repeat()` currently has **double protection** against oversubscription:

```r
# Line 133: Force ranger threading to 1
ranger.arguments$num.threads <- 1

# Line 158: Explicit n.cores = 1 when calling rf()
m.i <- spatialRF::rf(
  ...,
  ranger.arguments = ranger.arguments,  # already has num.threads = 1
  n.cores = 1,                          # redundant but explicit
  ...
)
```

**Result:** `n.cores` parameter in `rf_repeat()` controls ONLY the R-level parallelization (foreach workers), never ranger's threading.

#### Recommended Migration Strategy

**IMPORTANT:** Following the future ecosystem principle, spatialRF functions will NEVER call `plan()`.

**The ONLY correct approach:**

`n.cores` parameter controls **ranger threading only**. R-level parallelization is controlled by the user via `plan()`.

```r
rf_repeat <- function(model, repetitions = 10, n.cores = NULL) {
  # 1. Detect user's plan
  plan_workers <- nbrOfWorkers()

  # 2. Set intelligent default for n.cores
  if (is.null(n.cores)) {
    # If plan is parallel, disable ranger threading
    # If plan is sequential, use all cores for ranger
    n.cores <- if (plan_workers > 1) 1 else future::availableCores(omit = 1)
  }

  # 3. Warn about oversubscription
  if (plan_workers > 1 && n.cores > 1) {
    warning(
      "Parallel plan detected (", plan_workers, " workers) with n.cores = ", n.cores, ". ",
      "This creates ", plan_workers * n.cores, " total threads. ",
      "Consider setting n.cores = 1 to avoid oversubscription.",
      call. = FALSE
    )
  }

  # 4. Set ranger threading
  ranger.arguments$num.threads <- n.cores

  # 5. Use future.apply (respects user's plan)
  result <- future_lapply(seq_len(repetitions), function(i) {
    rf(
      data = data,
      ranger.arguments = ranger.arguments,  # Contains num.threads
      verbose = FALSE
    )
  })

  result
}
```

**This approach:**
- ✅ Never calls `plan()` - respects user control
- ✅ Detects user's plan and adapts automatically
- ✅ Provides intelligent defaults
- ✅ Warns about misconfiguration
- ✅ `n.cores` has clear, consistent meaning: ranger's num.threads
- ✅ Works correctly in all scenarios

**User scenarios:**

```r
# Scenario 1: User wants R-level parallelization (8 workers, ranger sequential)
plan(multisession, workers = 8)
result <- rf_repeat(model, repetitions = 100)  # n.cores defaults to 1

# Scenario 2: User wants ranger parallelization (sequential R, 8 ranger threads)
plan(sequential)  # or don't set plan at all
result <- rf_repeat(model, repetitions = 10, n.cores = 8)

# Scenario 3: User wants both (advanced - warned about oversubscription)
plan(multisession, workers = 4)
result <- rf_repeat(model, repetitions = 20, n.cores = 2)  # Gets warning: 4 × 2 = 8 threads

# Scenario 4: User wants sequential everything (debugging)
plan(sequential)
result <- rf_repeat(model, repetitions = 5, n.cores = 1)
```

**Migration impact - semantic change:**

In the **current** implementation (foreach):
- `n.cores` in `rf_repeat()`, `rf_evaluate()`, etc. controls **R-level workers** (foreach)
- ranger is always forced to `num.threads = 1`

In the **new** implementation (future):
- `n.cores` consistently controls **ranger's num.threads** everywhere
- R-level parallelization controlled by user's `plan()`
- Functions detect plan and auto-adjust ranger threading

This is a **semantic change** that requires clear documentation and migration guide for users.

---

### Example: Current vs Future Pattern

**Current: rf_evaluate() with foreach**

```r
rf_evaluate <- function(model, xy, repetitions = 30, n.cores = NULL, cluster = NULL) {
  # Extract model arguments
  ranger.arguments <- model$ranger.arguments

  # Force ranger to sequential when using R-level parallelization
  if (repetitions > 1) {
    ranger.arguments$num.threads <- 1  # Always 1 with foreach
  }

  # Setup parallel backend (creates cluster if needed)
  parallel_config <- setup_parallel_execution(cluster, n.cores)
  on.exit(parallel_config$cleanup(), add = TRUE)

  # Generate folds
  spatial.folds <- make_spatial_folds(...)

  # Parallel evaluation (n.cores controls foreach workers)
  evaluation.df <- foreach(i = seq_along(spatial.folds), .combine = "rbind") %dopar% {
    m.training <- spatialRF::rf(
      data = data.training,
      ranger.arguments = ranger.arguments,  # num.threads = 1
      n.cores = 1,  # Redundant but explicit
      verbose = FALSE
    )
    # Compute metrics...
  }

  evaluation.df
}
```

**Future: rf_evaluate() with future.apply**

```r
rf_evaluate <- function(model, xy, repetitions = 30, n.cores = NULL) {
  # Extract model arguments
  ranger.arguments <- model$ranger.arguments

  # Detect user's plan
  plan_workers <- nbrOfWorkers()

  # Set intelligent default for n.cores (controls ranger threading)
  if (is.null(n.cores)) {
    n.cores <- if (plan_workers > 1) 1 else future::availableCores(omit = 1)
  }

  # Warn about oversubscription
  if (plan_workers > 1 && n.cores > 1) {
    warning(
      "Parallel plan (", plan_workers, " workers) with n.cores = ", n.cores, " detected. ",
      "Total threads: ", plan_workers * n.cores, ". Consider n.cores = 1.",
      call. = FALSE
    )
  }

  # Set ranger threading (n.cores now controls ranger, not R-level parallelization)
  ranger.arguments$num.threads <- n.cores

  # Generate folds
  spatial.folds <- make_spatial_folds(...)

  # Parallel evaluation with progress (respects user's plan)
  evaluation.list <- with_progress({
    p <- progressor(along = spatial.folds)

    future_lapply(seq_along(spatial.folds), function(i) {
      m.training <- spatialRF::rf(
        data = data.training,
        ranger.arguments = ranger.arguments,  # Contains user's num.threads choice
        verbose = FALSE
      )

      p()  # Signal progress

      # Compute and return metrics...
    })
  })

  # Combine results
  evaluation.df <- do.call(rbind, evaluation.list)
  evaluation.df
}

# Usage - user controls R-level parallelization:
plan(multisession, workers = 8)       # 8 parallel folds
result <- rf_evaluate(model, xy)      # n.cores defaults to 1 (ranger sequential)

# Or - user wants ranger threading instead:
plan(sequential)                      # No R-level parallelization
result <- rf_evaluate(model, xy, n.cores = 8)  # ranger uses 8 threads
```

**Key differences:**
1. ❌ No `cluster` parameter - user sets `plan()` externally
2. ✅ Keep `n.cores` but **change meaning**: now controls ranger threading (was: R workers)
3. ❌ No `setup_parallel_execution()` - simpler, no cluster management
4. ✅ Detect plan with `nbrOfWorkers()` and adapt automatically
5. ✅ Native progressr integration
6. ⚠️ **SEMANTIC CHANGE:** `n.cores` meaning shifts from R-workers to ranger-threads

---

## Core Concepts

### What is a Future?

A **future** represents "a value that may be available at some point in the future." It exists in two states:

- **Unresolved**: Value not yet computed
- **Resolved**: Value immediately available

Key properties:
- Querying an unresolved future blocks execution until resolution
- Non-blocking status check: `resolved(f)`
- All futures provide consistent behavior regardless of backend

### Design Philosophy

The future framework provides:
1. **Unified abstraction** for asynchronous R expression evaluation
2. **Backend flexibility** without code changes
3. **Automatic variable handling** - globals identified and exported automatically
4. **Consistent behavior** across all execution strategies

---

## The future Package

### Two Future Styles

#### 1. Implicit Futures (Cleaner Syntax)

```r
# Using %<-% operator
v %<-% { expr }

# Closest to standard R assignment
result %<-% { expensive_computation() }
```

#### 2. Explicit Futures (Clearer Intent)

```r
# Using dedicated functions
f <- future({ expr })
v <- value(f)  # Blocks if unresolved

# Better communicates asynchronous intent
f <- future({ expensive_computation() })
result <- value(f)
```

**When to use each:**
- Implicit: Simpler workflows, cleaner code
- Explicit: Complex scenarios, loops, storing multiple futures

### Evaluation Strategies (Backends)

Configure with `plan()` - all maintain consistent API:

#### Sequential (Default - No Parallelization)

```r
plan(sequential)
```

- Evaluates synchronously in current R session
- Useful for development and debugging
- No overhead, single-threaded

#### Multisession (Cross-Platform Parallelization)

```r
plan(multisession, workers = 2)
```

**Characteristics:**
- Launches background R sessions on local machine
- Non-blocking after future creation
- Automatically exports globals to workers
- Worker count from `availableCores()`
- **Supported on all operating systems**

**When to use:** Default choice for parallelization on any platform

#### Multicore (Unix/Linux/macOS Only)

```r
plan(multicore)
```

**Characteristics:**
- Leverages process forking
- Reduced overhead through shared memory
- **Disabled by default in RStudio** (considered unstable)
- Unix/Linux/macOS only

**When to use:** Unix systems outside RStudio when you need lowest overhead

#### Cluster (Multi-Machine Distribution)

```r
# Simple hostname specification
plan(cluster, workers = c("n1", "n2", "n3"))

# Or explicit cluster creation
cl <- parallel::makeCluster(c("n1", "n2", "n3"))
plan(cluster, workers = cl)
```

**Characteristics:**
- Distributes across multiple machines
- SSH-based remote execution
- Fine-grained control with explicit cluster objects

**When to use:** HPC environments, remote computation

### The plan() Function

Controls evaluation backend with consistent API:

```r
# Switch backends freely
plan(sequential)        # Development
plan(multisession)      # Local parallel
plan(cluster, workers = hosts)  # Distributed
```

**Key insight:** Code works identically regardless of backend

### Nested Topologies

Specify different backends for different nesting levels:

```r
# Top-level parallel, nested sequential (prevents overload)
plan(list(multisession, sequential))

# Multiple parallel levels (explicitly controlled)
plan(list(
  tweak(multisession, workers = 2),
  tweak(multisession, workers = I(4))  # I() bypasses protection
))
```

**Resource protection:** By default, nested parallelism is restricted to prevent oversubscription. Use `I()` to explicitly enable controlled nested parallelism.

**Best practice:** `plan(list(tweak(multisession, workers = availableCores() %/% 4), tweak(multisession, workers = I(4))))`

### Global Variable Management

**Automatic behavior:**
- Globals automatically identified via static code inspection (using `globals` package)
- Package-defined globals: package attached instead of exported
- Size thresholds prevent accidental massive object exports
- Lazy evaluation freezes globals to local environments

**Manual specification when needed:**

```r
# By name
future(expr, globals = c("a", "slow_sum"))

# As name-value pairs
future(expr, globals = list(a = 42, slow_sum = my_sum))

# Add to automatically detected globals
future(expr) %globals% structure(TRUE, add = "extra_var")
```

**Package specification:**

```r
# When packages aren't auto-detected
future(expr) %packages% "data.table"
```

### Error Handling

Errors propagate when values are requested:

```r
a %<-% { log("hello") }  # Error deferred
a  # Error thrown here when value accessed

# Debug with backtrace
backtrace(a)
```

### Consistent Behavior Guarantees

All futures provide:
1. **Local evaluation environment** - assignments don't affect calling scope
2. **Global identification and export** - proper variable capture
3. **Single evaluation** - results cached after first computation

---

## The future.apply Package

### Purpose

Provides parallel alternatives to base R apply functions with minimal code changes.

### Core Philosophy

> "The purpose of this package is to provide worry-free parallel alternatives to base-R apply functions."

**API constraint:** Stays aligned with base R - no expansion beyond core apply functions.

### Available Functions

Direct replacements for base R:

| Base R | future.apply |
|--------|--------------|
| `lapply()` | `future_lapply()` |
| `sapply()` | `future_sapply()` |
| `vapply()` | `future_vapply()` |
| `apply()` | `future_apply()` |
| `mapply()` | `future_mapply()` |
| `replicate()` | `future_replicate()` |
| `tapply()` | `future_tapply()` |
| `by()` | `future_by()` |
| `eapply()` | `future_eapply()` |
| `Map()` | `future_Map()` |

**Note:** `future_rapply()` not implemented.

### Basic Usage Pattern

**Sequential (base R):**

```r
library(datasets)
library(stats)
y <- lapply(mtcars, FUN = mean, trim = 0.10)
```

**Parallel (future.apply):**

```r
library(future.apply)
plan(multisession)  # Configure backend once

library(datasets)
library(stats)
y <- future_lapply(mtcars, FUN = mean, trim = 0.10)
```

**Key insight:** Drop-in replacement with identical syntax.

### Reproducibility: Parallel RNG

Built-in support for reproducible random number generation:

```r
# Ensures consistent results regardless of chunking/load-balancing
future_lapply(x, FUN = function(i) {
  rnorm(1)
}, future.seed = TRUE)
```

**Important:** Use `future.seed = TRUE` whenever RNG is involved in parallel operations.

### Backend Flexibility

Works with entire future ecosystem:

- Built-in: `parallel`
- Extensions: `future.callr`, `future.mirai`, `future.batchtools`
- HPC schedulers: LSF, SLURM, SGE via `future.batchtools`

Switch backends by changing `plan()` only - no code changes needed.

---

## Integration with progressr

### Overview

The **progressr** package provides progress reporting that works seamlessly with future-based parallelization.

**Key design:** Separates progress signal representation from presentation:
- **Developers** add progress tracking
- **End users** control how/when/where progress displays

### Developer API

Add progress tracking to functions:

```r
slow_sum <- function(x) {
  # Create progressor
  p <- progressr::progressor(along = x)

  sum <- 0
  for (kk in seq_along(x)) {
    Sys.sleep(0.1)
    sum <- sum + x[kk]

    # Signal progress
    p(message = sprintf("Adding %g", x[kk]))
  }
  sum
}
```

**Progressor creation options:**

```r
p <- progressor(nsteps)           # Fixed number of steps
p <- progressor(along = x)        # Based on collection length
```

**Signaling progress:**

```r
p()                    # Increment by one step
p(amount = 0)         # "Still alive" signal (no increment)
p("loading ...")      # Include a message
```

### User API

Users control progress presentation:

```r
# Enable globally
library(progressr)
handlers(global = TRUE)

# Or scope to specific expressions
with_progress({
  y <- slow_sum(1:5)
})

# Configure handlers
handlers("txtprogressbar")                      # Basic text bar
handlers("progress")                            # Enhanced progress bar
handlers("txtprogressbar", "beepr")            # Multiple handlers
handlers(handler_pbcol(enable_after = 3.0))    # Delayed activation
```

### Integration with future.apply

Two usage patterns:

#### Pattern 1: Package Functions

```r
my_analysis <- function(x) {
  # Create progressor inside function
  p <- progressor(steps = length(x))

  future_lapply(x, function(item) {
    p()  # Signal after each iteration
    Sys.sleep(0.2)
    sum(item)
  })
}

# User wraps the call
with_progress({
  result <- my_analysis(data)
})
```

#### Pattern 2: Interactive Scripts

```r
# Create progressor inside with_progress()
with_progress({
  p <- progressor(steps = length(x))

  result <- future_lapply(x, function(item) {
    p()
    expensive_computation(item)
  })
})
```

**Critical constraint:** Progressor must be created **inside** `with_progress()` block.

### Available Handlers

- **Visual:** `"cli"`, `"progress"`, `"txtprogressbar"`, `"winProgressBar"`, `"tkProgressBar"`
- **Auditory:** `"beepr"`
- **Notifications:** `"ntfy"`, `"shiny::withProgress"`
- **File system:** Track via file size
- **Custom:** User-defined handlers

### Advanced Features

**Non-interfering output:**
- `message()`, `cat()`, `print()` don't disrupt progress bars
- Framework buffers output internally

**Sticky messages:**

```r
p("Checkpoint reached", class = "sticky")
```

Messages pushed upward with terminal output - useful for milestones.

---

## Migration Patterns

### From foreach+doParallel to future.apply

#### Old Pattern (foreach+doParallel)

```r
library(foreach)
library(doParallel)

# Setup cluster
cl <- makeCluster(n.cores)
registerDoParallel(cl)

# Parallel loop
result <- foreach(
  i = 1:n,
  .combine = 'c',
  .packages = c("pkg1", "pkg2"),
  .export = c("var1", "var2")
) %dopar% {
  expensive_computation(i)
}

# Cleanup
stopCluster(cl)
```

#### New Pattern (future.apply)

```r
library(future.apply)

# Setup backend (once, often at top of script/function)
plan(multisession, workers = n.cores)

# Parallel computation
result <- future_lapply(1:n, function(i) {
  expensive_computation(i)
})

# No explicit cleanup needed (automatic)
# Only stop user-provided clusters: if (!is.null(user_cluster)) stopCluster(user_cluster)
```

### Key Differences

| Aspect | foreach+doParallel | future.apply |
|--------|-------------------|--------------|
| **Setup** | `makeCluster()` + `registerDoParallel()` | `plan()` |
| **Backend config** | Backend-specific registration | Universal `plan()` |
| **Globals** | Manual `.export` | Automatic detection |
| **Packages** | Manual `.packages` | Automatic (or `%packages%`) |
| **Cleanup** | Manual `stopCluster()` | Automatic (unless user-provided) |
| **Syntax** | Custom `%dopar%` operator | Standard function calls |
| **Combine** | `.combine` argument | Use appropriate function or post-process |

### Common Translations

#### Simple foreach loop

```r
# OLD
result <- foreach(i = 1:n, .combine = 'c') %dopar% {
  compute(i)
}

# NEW
result <- unlist(future_lapply(1:n, compute))
# or
result <- future_sapply(1:n, compute)
```

#### With data frame row processing

```r
# OLD
result <- foreach(i = 1:nrow(df), .combine = 'rbind') %dopar% {
  process_row(df[i, ])
}

# NEW
result <- do.call(rbind, future_lapply(1:nrow(df), function(i) {
  process_row(df[i, ])
}))
```

#### With exports and packages

```r
# OLD
result <- foreach(
  i = 1:n,
  .export = c("helper_fn", "data_obj"),
  .packages = "specialpkg"
) %dopar% {
  helper_fn(i, data_obj)
}

# NEW
# Usually automatic, but if needed:
result <- future_lapply(1:n, function(i) {
  helper_fn(i, data_obj)
}) %globals% structure(TRUE, add = c("helper_fn", "data_obj")) %packages% "specialpkg"

# Or let it auto-detect (preferred)
result <- future_lapply(1:n, function(i) {
  helper_fn(i, data_obj)
})
```

### Cluster Management Pattern

**Current spatialRF pattern (foreach):**

```r
my_function <- function(data, n.cores = NULL, cluster = NULL) {
  # Setup
  if (is.null(cluster)) {
    if (is.null(n.cores)) n.cores <- future::availableCores(omit = 1)
    cluster <- parallel::makeCluster(n.cores)
    cluster.local <- TRUE
  } else {
    cluster.local <- FALSE
  }

  doParallel::registerDoParallel(cluster)

  # Work
  result <- foreach(...) %dopar% { ... }

  # Cleanup
  if (cluster.local) {
    parallel::stopCluster(cluster)
  }

  result
}
```

**Recommended future pattern:**

```r
my_function <- function(data, n.cores = NULL) {
  # User configures plan() externally - we just use it
  # No cluster creation/management in function

  # Work (respects user's plan)
  result <- future_lapply(seq_len(nrow(data)), function(i) {
    process(data[i, ])
  })

  result
}

# User controls parallelization:
plan(sequential)                    # Development/debugging
plan(multisession, workers = 4)    # Local parallel
plan(cluster, workers = my_cluster) # Custom cluster
```

**Alternative (backward compatible):**

```r
my_function <- function(data, n.cores = NULL) {
  # Set default plan if not already configured
  if (is.null(n.cores)) {
    # Use existing plan (user's choice)
  } else {
    # Override with user's n.cores
    old_plan <- plan()
    on.exit(plan(old_plan), add = TRUE)
    plan(multisession, workers = n.cores)
  }

  result <- future_lapply(seq_len(nrow(data)), function(i) {
    process(data[i, ])
  })

  result
}
```

---

## Best Practices & Pitfalls

### Globals: Common Issues

#### 1. Conditional Local Assignments (False Negative)

**Problem:**

```r
reset <- FALSE
x <- 1
y %<-% { if (reset) x <- 0; x + 1 }  # Fails to detect x as global
```

**Solution:**

```r
y %<-% { x; if (reset) x <- 0; x + 1 }  # Reference x at start
```

#### 2. Functions via do.call()

**Problem:**

```r
do.call("file_ext", list("foo.txt"))  # String - not detected
```

**Solution:**

```r
do.call(file_ext, list("foo.txt"))  # Function object - detected
```

#### 3. Variables in get()

**Problem:**

```r
y %<-% { get("a") + 1 }  # "a" as string - not detected
```

**Solutions:**

```r
# Explicit declaration
y %<-% { get("a") + 1 } %globals% structure(TRUE, add = "a")

# Or inject
y %<-% { a; get("a") + 1 }

# Better: refactor to avoid get()
y %<-% { a + 1 }
```

#### 4. String Interpolation (glue, sprintf)

**Problem:**

```r
library(glue)
s %<-% glue("Value: {a}")  # Variable hidden in string
```

**Solutions:**

```r
# Explicit globals
s %<-% glue("Value: {a}") %globals% structure(TRUE, add = "a")

# Or inject
s %<-% { a; glue("Value: {a}") }
```

### The ... (ellipsis) Pitfall

**Problem:**

```r
my_fcn <- function(X, ...) {
  future_lapply(X, FUN = function(x) {
    round(x, ...)  # ERROR: '...' used in incorrect context
  })
}
```

**Solution:**

```r
my_fcn <- function(X, ...) {
  future_lapply(X, FUN = function(x, ...) {
    round(x, ...)
  }, ...)  # Pass ... explicitly through all levels
}
```

**Key insight:** Pass `...` explicitly through function calls, don't rely on it as a global.

### Missing Packages

**Problem:**
S3 methods may not auto-detect required packages.

```r
# data.table example
DT <- data.table::data.table(a = 1:5, b = 5:1)
y %<-% DT[, sum(b)]  # May fail if data.table not loaded in worker
```

**Solution:**

```r
y %<-% DT[, sum(b)] %packages% "data.table"
```

**Important:** Avoid `library()` inside futures - use `%packages%` instead.

### Non-Exportable Objects

Some objects cannot be passed to workers:
- XML objects
- External pointers
- Database connections
- Some S4 objects

**Detection:**

```r
options(future.globals.onReference = "error")
```

### Operator Precedence

**Problem:**

```r
x %<-% 2 * runif(1)  # ERROR: precedence issue
```

**Solution:**

```r
x %<-% { 2 * runif(1) }  # Always use braces
```

**With pipes:**

```r
x %<-% { 1:100 %>% sum }  # Brace the entire expression
```

### Error Handling in Futures

**Problem:**

```r
res <- tryCatch({
  unstable_calc(x)
}, error = function(e) {
  warning(e)  # Passing error object directly causes issues
  NA_real_
})
```

**Solution:**

```r
res <- tryCatch({
  unstable_calc(x)
}, error = function(e) {
  warning(conditionMessage(e))  # Extract message
  NA_real_
})
```

### Package Development Notes

1. **Source scripts at top level**, not inside futures
2. If sourcing in future is necessary: `source("script.R", local = TRUE)`
3. Use dummy assignments to silence `R CMD check` NOTEs about unbound variables
4. Let users configure `plan()` - don't force it in your package

### Operator Masking

If other packages define conflicting operators (e.g., igraph's `%<-%`):

```r
# Load future last
library(igraph)
library(future)

# Or explicitly import
`%<-%` <- future::`%<-%`
```

---

## Comparison: foreach vs future.apply

### Philosophy

| Aspect | foreach | future.apply |
|--------|---------|--------------|
| **Design goal** | Custom parallel loop syntax | Drop-in replacements for base R |
| **API style** | Domain-specific (iterators) | Functional (apply family) |
| **Backend coupling** | Backend-specific registration | Universal abstraction |
| **Learning curve** | New syntax, concepts | Minimal (if you know apply) |

### Syntax Comparison

#### Iterating with side effects

```r
# foreach
foreach(i = 1:10) %dopar% {
  write.csv(process(i), file = paste0("out_", i, ".csv"))
}

# future.apply
invisible(future_lapply(1:10, function(i) {
  write.csv(process(i), file = paste0("out_", i, ".csv"))
}))
```

#### Combining results

```r
# foreach - built-in combine
result <- foreach(i = 1:10, .combine = 'c') %dopar% {
  compute(i)
}

# future.apply - explicit combine
result <- unlist(future_lapply(1:10, compute))
# or
result <- future_sapply(1:10, compute)
```

#### Complex combine operations

```r
# foreach
result <- foreach(i = 1:10, .combine = 'rbind') %dopar% {
  data.frame(id = i, value = compute(i))
}

# future.apply
result <- do.call(rbind, future_lapply(1:10, function(i) {
  data.frame(id = i, value = compute(i))
}))
```

### Feature Comparison

| Feature | foreach | future.apply |
|---------|---------|--------------|
| **Global detection** | Manual (`.export`) | Automatic |
| **Package loading** | Manual (`.packages`) | Automatic (or `%packages%`) |
| **Nested parallelism** | Complex | Built-in with `plan(list(...))` |
| **Progress reporting** | Via doSNOW, external | Native via progressr |
| **RNG reproducibility** | Backend-dependent | Built-in via `future.seed` |
| **Cleanup** | Manual `stopCluster()` | Automatic |
| **Backend switching** | Re-register | Change `plan()` |

### Performance Considerations

Both frameworks have similar performance characteristics when properly configured. Key factors:

1. **Overhead:** Both have startup/marshalling costs - only worthwhile for non-trivial computations
2. **Chunking:** future.apply auto-chunks; foreach processes one-by-one unless using `.inorder = FALSE`
3. **Load balancing:** Both support it, future.apply's is more automatic

### Migration Recommendation

For spatialRF (and similar packages):

1. **Primary choice:** future.apply
   - Simpler API
   - Automatic variable handling reduces bugs
   - Unified with modern R ecosystem (tidyverse uses furrr, which is future-based)
   - Better progress reporting integration

2. **Use foreach if:**
   - Already deeply integrated and working well
   - Need very specific iterator patterns
   - Backend-specific features required

---

## Quick Reference Card

### Setup & Execution

```r
# Load packages
library(future.apply)
library(progressr)

# Configure backend
plan(sequential)                      # No parallelization (default)
plan(multisession, workers = 4)      # Local parallel (cross-platform)
plan(multicore)                      # Forking (Unix only, not RStudio)
plan(cluster, workers = c("n1", "n2"))  # Multi-machine

# Basic parallel apply
result <- future_lapply(x, function(item) {
  expensive_computation(item)
})

# With reproducible RNG
result <- future_lapply(x, function(item) {
  rnorm(10)
}, future.seed = TRUE)

# With progress
with_progress({
  p <- progressor(along = x)
  result <- future_lapply(x, function(item) {
    p()
    expensive_computation(item)
  })
})
```

### Common Functions

```r
# future.apply
future_lapply()     # Returns list
future_sapply()     # Simplifies result
future_vapply()     # Type-safe sapply
future_apply()      # Array margins
future_replicate()  # Repeated evaluation

# Plan detection and resource management
nbrOfWorkers()                  # Get number of workers (1 = sequential, >1 = parallel)
nbrOfFreeWorkers()              # Get available workers not in use
plan()                          # Get current plan (call without args)
inherits(plan(), "sequential")  # Check if sequential
inherits(plan(), "multisession") # Check if multisession
availableCores()            # Get available cores (better than detectCores)
availableCores(omit = 1)    # Reserve 1 core (safer default)

# progressr
progressor(nsteps)              # Create progressor
progressor(along = x)           # Based on length
p()                             # Signal progress
p(message = "text")             # With message
with_progress({ code })         # Enable progress
handlers(global = TRUE)         # Enable globally
handlers("txtprogressbar")      # Configure handler
```

### Troubleshooting

```r
# Debug missing globals
options(future.globals.onReference = "error")

# Manual global specification
future(expr) %globals% structure(TRUE, add = c("var1", "var2"))

# Manual package specification
future(expr) %packages% c("pkg1", "pkg2")

# Check if future is resolved
resolved(f)

# Get future value (blocks if needed)
v <- value(f)

# Reset to sequential for debugging
plan(sequential)
```

---

## Sources

- future package website: https://future.futureverse.org/
- future.apply package website: https://future.apply.futureverse.org/
- progressr package website: https://progressr.futureverse.org/
- CRAN future vignette: https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
- CRAN future topologies: https://cran.r-project.org/web/packages/future/vignettes/future-3-topologies.html
- CRAN future issues: https://cran.r-project.org/web/packages/future/vignettes/future-4-issues.html
- furrr progress guide: https://furrr.futureverse.org/articles/progress.html
- CRAN future.apply overview: https://cran.r-project.org/web/packages/future.apply/vignettes/future.apply-1-overview.html
- Web search: future package R progressr integration progress bars 2025

---

## Notes for spatialRF Migration

### Current State (foreach+doParallel)

- Manual cluster creation/management in functions
- `n.cores` and `cluster` parameters throughout
- `doParallel::registerDoParallel()`
- Manual cleanup with `stopCluster()`
- `.export` and `.packages` arguments

### Target State (future+future.apply)

- User controls `plan()` externally (or package provides defaults)
- Functions remain cluster-agnostic
- Automatic variable detection
- Automatic cleanup
- Simpler function signatures
- Native progress with progressr

### Migration Strategy

1. **Phase 1:** Replace foreach loops with future_lapply
2. **Phase 2:** Simplify cluster management (remove cluster parameter, let users control plan)
3. **Phase 3:** Add progressr integration
4. **Phase 4:** Remove doParallel dependency

### Backward Compatibility

Consider keeping `n.cores` parameter for backward compatibility, but use it to configure plan():

```r
my_function <- function(data, n.cores = NULL) {
  if (!is.null(n.cores)) {
    old_plan <- plan()
    on.exit(plan(old_plan), add = TRUE)
    plan(multisession, workers = n.cores)
  }
  # else: respect existing plan

  # Use future.apply functions
  result <- future_lapply(...)
  result
}
```

This allows:
- Users who don't specify n.cores to control via plan() directly
- Users who specify n.cores get expected behavior
- Smooth transition path
