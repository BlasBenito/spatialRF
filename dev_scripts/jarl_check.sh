#!/bin/bash
# Helper script for running jarl linter on spatialRF
# Usage: ./dev_scripts/jarl_check.sh [--fix] [--unsafe-fixes]

JARL=~/.local/bin/jarl

# Default options for spatialRF
# Using `<-` for assignment (R standard)
# Checking all rules
SELECT="ALL"
ASSIGNMENT="<-"

# Parse arguments
FIX_FLAG=""
UNSAFE_FLAG=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --fix)
      FIX_FLAG="--fix"
      shift
      ;;
    --unsafe-fixes)
      UNSAFE_FLAG="--unsafe-fixes"
      shift
      ;;
    --help|-h)
      echo "Usage: $0 [--fix] [--unsafe-fixes]"
      echo ""
      echo "Options:"
      echo "  --fix            Auto-fix issues"
      echo "  --unsafe-fixes   Include potentially unsafe fixes"
      echo "  -h, --help       Show this help"
      echo ""
      echo "Examples:"
      echo "  $0                    # Check only"
      echo "  $0 --fix              # Check and auto-fix safe issues"
      echo "  $0 --fix --unsafe-fixes  # Check and auto-fix all issues"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      echo "Use --help for usage information"
      exit 1
      ;;
  esac
done

# Run jarl
echo "Running jarl linter on R/ directory..."
echo "Assignment operator: $ASSIGNMENT"
echo "Rules: $SELECT"
[[ -n "$FIX_FLAG" ]] && echo "Auto-fix: enabled"
[[ -n "$UNSAFE_FLAG" ]] && echo "Unsafe fixes: enabled"
echo ""

$JARL check R/ \
  --select "$SELECT" \
  --assignment "$ASSIGNMENT" \
  $FIX_FLAG \
  $UNSAFE_FLAG
