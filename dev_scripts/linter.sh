#!/usr/bin/env bash
jarl check R/ \
  --select style,complexity,robustness \
  --ignore line-length,object-name,comment-format,todo-comment \
  --fix
