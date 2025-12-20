#!/usr/bin/env bash
jarl check R/ \
  --select style,complexity,robustness \
  --assignment "<-" \
  --fix
