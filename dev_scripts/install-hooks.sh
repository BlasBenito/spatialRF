#!/usr/bin/env bash

# Install Git hooks for spatialRF development
# Usage: bash dev_scripts/install-hooks.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIT_DIR="$(git rev-parse --git-dir 2>/dev/null)"

if [ -z "$GIT_DIR" ]; then
    echo "❌ Error: Not in a git repository"
    exit 1
fi

echo "Installing pre-commit hook..."
cp "$SCRIPT_DIR/pre-commit.template" "$GIT_DIR/hooks/pre-commit"
chmod +x "$GIT_DIR/hooks/pre-commit"

echo "✅ Pre-commit hook installed successfully!"
echo ""
echo "The hook will run before each commit to:"
echo "  1. Check for merge conflicts and debugging statements"
echo "  2. Update roxygen documentation"
echo "  3. Run jarl linter"
echo "  4. Check spelling (warning only, doesn't block)"
echo "  5. Run critical tests (rf and rf_spatial only)"
echo "  6. Run R CMD check (without full test suite)"
echo "  7. Remind to update NEWS.md when R/ files change"
echo ""
echo "To skip the hook temporarily, use: git commit --no-verify"
