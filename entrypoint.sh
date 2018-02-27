#!/bin/bash

STORE=$(realpath /mddb/"${STORE:-cs.repo}")
MDDB="/mddb/${MDDB:-metadata.db}"
RECIPES="${RECIPES:-/recipes/}"

/usr/local/bin/bdcs-api-server --bdcs "$STORE" "$MDDB" "$RECIPES"
