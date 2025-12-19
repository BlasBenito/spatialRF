# CRAN Compliance Agent

A custom Claude agent built with the Claude Agent SDK that automatically
checks R packages for CRAN submission compliance, identifying potential
NOTEs, WARNINGs, and ERRORs before submission.

## Features

- **Comprehensive Compliance Checks**: Reviews all aspects of CRAN
  policy
- **DESCRIPTION Validation**: Checks required fields, format, and
  content
- **Documentation Completeness**: Verifies all exports are documented
- **Example Quality**: Identifies timing issues, side effects, and
  improper `\dontrun{}` usage
- **Code Review**: Finds anti-patterns that violate CRAN policies
- **Automated R CMD check**: Optionally runs devtools::check() to catch
  real issues
- **Detailed Reporting**: Categorizes issues by severity with specific
  fixes

## Installation

Same requirements as Roxy agent:

``` bash
# The SDK is already installed if you set up Roxy
# If not:
pip install claude-agent-sdk
```

## Usage

### Check Current Package

Run from your package root directory:

``` bash
python3 cran_agent.py
```

### Check Specific Package

Specify the package directory:

``` bash
python3 cran_agent.py /path/to/your/package
```

## What It Checks

### 1. DESCRIPTION File

- [x] Required fields present and properly formatted
- [x] Title in title case, no trailing period
- [x] Description has multiple sentences
- [x] Version format (X.Y.Z)
- [x] License is CRAN-compatible
- [x] URL/BugReports are valid
- [x] Dependencies properly listed

### 2. Documentation

- [x] All exported functions documented
- [x] All parameters have `@param` tags
- [x] Return values documented with `@return`
- [x] Examples present and meaningful
- [x] Datasets in `data/` are documented

### 3. Examples

- [x] Run in \< 5 seconds
- [x] Minimal use of `\dontrun{}` (should use `\donttest{}` instead)
- [x] No writes to user’s file system (should use
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html))
- [x] No internet dependency without `\donttest{}`
- [x] Graphics parameters restored with
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html)

### 4. R Code Quality

- [x] No [`browser()`](https://rdrr.io/r/base/browser.html),
  [`print()`](https://rdrr.io/r/base/print.html), or
  [`cat()`](https://rdrr.io/r/base/cat.html) in functions
- [x] No [`setwd()`](https://rdrr.io/r/base/getwd.html) calls
- [x] No direct access to `.GlobalEnv`
- [x] Proper use of [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  for temporary files
- [x] No direct writes to home directory

### 5. NAMESPACE

- [x] Proper exports for user-facing functions
- [x] Proper imports (no `::` without importing)
- [x] No unnecessary `.onLoad`/`.onAttach`

### 6. Package Structure

- [x] Standard directory organization
- [x] Reasonable package size (\< 5 MB)
- [x] No unnecessary large files
- [x] Tests use [`tempdir()`](https://rdrr.io/r/base/tempfile.html) and
  handle missing packages

### 7. Cross-Platform Compatibility

- [x] Path handling with
  [`file.path()`](https://rdrr.io/r/base/file.path.html)
- [x] No shell-specific commands
- [x] Windows/Mac/Linux compatibility

### 8. CRAN Policy Compliance

- [x] No direct calls to
  [`installed.packages()`](https://rdrr.io/r/utils/installed.packages.html)
- [x] Proper resource usage
- [x] No unexported objects in examples
- [x] Core functionality not in `\dontrun{}`

## Example Output

    Checking CRAN compliance for package in: /path/to/spatialRF
    ============================================================

    ============================================================
    CRAN COMPLIANCE CHECK
    ============================================================

    Reviewing DESCRIPTION file...

    ✓ All required fields present
    ⚠ WARNING: Title should not end with a period
      Location: DESCRIPTION:3
      Fix: Remove trailing period from Title field

    Checking documentation completeness...

    ✓ All 45 exported functions are documented
    ⚠ NOTE: Function 'rf_spatial' example may take > 5 seconds
      Location: man/rf_spatial.Rd:25-35
      Fix: Reduce example complexity or wrap slow parts in \donttest{}

    Reviewing R code for anti-patterns...

    ✗ ERROR: Use of browser() found
      Location: R/rf_tuning.R:145
      Fix: Remove debugging call before submission
      CRAN Policy: Interactive debugging calls not allowed

    Summary:
    - 1 ERROR (must fix)
    - 1 WARNING (should fix)
    - 1 NOTE (consider fixing)
    - 5 SUGGESTIONS (optional improvements)

    ============================================================
    ✓ CRAN compliance check complete!
    ============================================================

## Integration with Workflow

Recommended workflow for CRAN submission:

``` bash
# 1. Fix documentation quality
python3 roxy_agent.py

# 2. Update documentation
Rscript -e "devtools::document()"

# 3. Check CRAN compliance
python3 cran_agent.py

# 4. Run R CMD check
Rscript -e "devtools::check()"

# 5. Build for CRAN
Rscript -e "devtools::build()"

# 6. Final check as CRAN
R CMD check --as-cran packagename_version.tar.gz
```

## Comparison with Roxy

| Feature         | Roxy                          | CRAN Agent                   |
|-----------------|-------------------------------|------------------------------|
| **Focus**       | Documentation grammar/clarity | CRAN policy compliance       |
| **Edits**       | Fixes documentation text      | Reviews but doesn’t auto-fix |
| **Scope**       | Roxygen comments only         | Entire package structure     |
| **Output**      | Updated .R files              | Compliance report            |
| **When to use** | During development            | Before CRAN submission       |

Both agents complement each other: - **Roxy** makes documentation clear
and professional - **CRAN Agent** ensures it meets CRAN requirements

## Severity Levels

The agent reports issues at four levels:

- **ERROR**: Will prevent CRAN acceptance (e.g.,
  [`browser()`](https://rdrr.io/r/base/browser.html) calls, missing
  required fields)
- **WARNING**: May prevent acceptance or generate WARNINGs in R CMD
  check
- **NOTE**: Will generate NOTEs in R CMD check (CRAN may ask for
  explanation)
- **SUGGESTION**: Best practices that improve package quality

## Common Issues Found

### Most Common ERRORs

1.  [`browser()`](https://rdrr.io/r/base/browser.html) or debugging code
    left in functions
2.  Missing required DESCRIPTION fields
3.  Undocumented exported functions
4.  Examples that fail to run

### Most Common WARNINGs

1.  Title ending with period
2.  Invalid URLs in DESCRIPTION
3.  Undocumented parameters
4.  Missing @return documentation

### Most Common NOTEs

1.  Examples taking \> 5 seconds
2.  Excessive use of `\dontrun{}`
3.  Unexported objects in examples
4.  Package size \> 5 MB

## Configuration

The agent uses these settings by default:

``` python
ClaudeAgentOptions(
    allowed_tools=["Read", "Glob", "Grep", "Bash"],
    permission_mode="default",  # Interactive approval
    system_prompt="You are a CRAN compliance specialist..."
)
```

To modify behavior, edit `cran_agent.py` and adjust: - `allowed_tools`:
Add/remove tools - `permission_mode`: Change to `"acceptAll"` for
auto-approval of reads - `system_prompt`: Customize checking priorities

## Running R CMD Check

The agent can optionally run
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
to catch actual R CMD check issues. This requires:

- R installed and in PATH
- `devtools` package installed
- Write permissions in package directory

## Troubleshooting

**“No DESCRIPTION file found”** - Run from package root directory (where
DESCRIPTION is) - Or specify full path:
`python3 cran_agent.py /path/to/package`

**“Permission denied” errors** - The agent respects permission_mode
settings - Review and approve read operations when prompted

**Agent suggests running devtools::check() but fails** - Ensure R is
installed: `which R` - Ensure devtools is installed:
`Rscript -e "library(devtools)"` - Check package is buildable:
`Rscript -e "devtools::load_all()"`

## Learn More

- [CRAN Repository
  Policy](https://cran.r-project.org/web/packages/policies.html)
- [Writing R
  Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
- [R Package Development](https://r-pkgs.org/)
- [Common CRAN Issues](https://blog.r-hub.io/2020/12/01/cran-checks/)

## Contributing

Found an issue or want to add more checks? Edit the system prompt in
`cran_agent.py` to add: - Additional policy checks - Specific
anti-patterns - Custom validation rules - Integration with other tools
