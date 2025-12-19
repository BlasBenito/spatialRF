# Roxy - Roxygen Documentation Review Agent

A custom Claude agent built with the Claude Agent SDK that automatically
reviews and fixes grammar and language errors in roxygen documentation
across your R package.

## Features

- **Automated Review**: Scans all R files in your package
- **Grammar Correction**: Identifies and fixes grammatical errors
- **Clarity Improvements**: Simplifies verbose or unclear documentation
- **Consistency**: Ensures uniform style across all documentation
- **Interactive**: Asks for approval before making changes
- **Detailed Reporting**: Explains what changes were made and why

## Installation

### Prerequisites

- Python 3.8 or higher
- Claude Code authentication (or ANTHROPIC_API_KEY)

### Setup

1.  Install the Claude Agent SDK:

``` bash
# Using pip
pip install claude-agent-sdk

# Or using uv (recommended)
uv pip install claude-agent-sdk
```

2.  Authenticate with Claude:

``` bash
# If you have Claude Code installed
claude  # Just run once to authenticate

# Or set your API key
export ANTHROPIC_API_KEY=your-api-key
```

## Usage

### Review All R Files

Review all roxygen documentation in the `R/` directory:

``` bash
python3 roxy_agent.py
```

### Review Specific Directory

Review all R files in a specific directory:

``` bash
python3 roxy_agent.py path/to/R/directory
```

### Review Single File

Review a specific R file:

``` bash
python3 roxy_agent.py R/my_function.R
```

## How It Works

1.  **Scans** R files for roxygen documentation blocks (lines starting
    with `#'`)
2.  **Analyzes** each documentation section:
    - `@title` and `@description`: Clarity and conciseness
    - `@param`: Parameter descriptions and consistency
    - `@return`: Return value documentation
    - `@details`: Additional context
    - `@examples`: Code examples
3.  **Identifies** issues:
    - Grammar errors
    - Unclear or verbose language
    - Inconsistent terminology
    - Redundant information
4.  **Fixes** problems while maintaining:
    - Technical accuracy
    - Roxygen2 format
    - Existing structure
5.  **Reports** all changes made with explanations

## Configuration

The agent is configured with these defaults:

- **Permission Mode**: `default` (asks before making edits)
- **Allowed Tools**: Read, Edit, Glob, Grep
- **Working Directory**: Parent directory of R files

To modify the agent’s behavior, edit `roxy_agent.py` and adjust the
`ClaudeAgentOptions`.

## Example Output

    Found 65 R files to review
    ============================================================

    [1/65] Reviewing: rf.R
    ------------------------------------------------------------
    I found several improvements to make in the roxygen documentation:

    1. Line 2 (@description): Changed "rmse and nrmse" to "RMSE and NRMSE"
    2. Line 6 (@param distance.matrix): Changed "Squared matrix" to "Square matrix"
    3. Line 7: Fixed typo "dustance.thresholds" → "distance.thresholds"
    ...

    ============================================================
    ✓ Roxygen documentation review complete!
      Reviewed 65 files in ./R/

## Customization

### Custom System Prompt

Edit the `system_prompt` in `roxy_agent.py` to adjust the agent’s
behavior:

``` python
system_prompt="""You are Roxy, a specialist in R package documentation.
Add your custom instructions here..."""
```

### Auto-Approve Edits

Change permission mode to skip approval prompts:

``` python
permission_mode="acceptEdits"  # Auto-approve all edits
```

### Add Custom Tools

Import and register custom MCP tools in the agent configuration.

## Best Practices

- **Run on a clean git branch** so you can review changes with
  `git diff`
- **Review changes before committing** - the agent makes suggestions,
  you make final decisions
- **Run after major updates** to maintain documentation quality
- **Combine with devtools::check()** to ensure documentation builds
  correctly

## Troubleshooting

**Agent not finding files:** - Ensure you’re in the package root
directory - Check the path argument is correct

**Authentication errors:** - Verify Claude Code is authenticated:
`claude` - Or set `ANTHROPIC_API_KEY` environment variable

**Permission errors:** - Ensure files are writable - Check file
permissions with `ls -la R/`

## Integration with Development Workflow

Add to your package development workflow:

``` bash
# 1. Make code changes
# 2. Update roxygen docs
# 3. Run Roxy agent
python3 roxy_agent.py

# 4. Generate documentation
Rscript -e "devtools::document()"

# 5. Check package
Rscript -e "devtools::check()"

# 6. Review and commit
git diff
git add .
git commit -m "Update documentation"
```

## Learn More

- [Claude Agent SDK
  Documentation](https://platform.claude.com/docs/en/agent-sdk/overview)
- [Roxygen2 Documentation](https://roxygen2.r-lib.org/)
- [R Package Development](https://r-pkgs.org/)
