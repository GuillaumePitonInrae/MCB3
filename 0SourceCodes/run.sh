#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
INPUT_JSON=$(realpath "$1")
OUTPUT_DIR=$(realpath "$2")
cd $SCRIPT_DIR # R needs CWD to be set in the source files dir

echo "Running DFBuffering R script"
echo "Working dir is set to '"$(pwd)"'"
echo "Input parameters will be loaded from '$INPUT_JSON'"
echo "Output files will be written at '$OUTPUT_DIR'"

RSCRIPT_CMD="Rscript '00_MainCode.R' $INPUT_JSON $OUTPUT_DIR"
echo "Now executing R with command '$RSCRIPT_CMD'"
eval "$RSCRIPT_CMD"