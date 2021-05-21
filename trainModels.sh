#!/usr/bin/env bash

MODFILES="*knnV*.R"

trap "exit" INT
# Get data needed for models training.
Rscript fetchData.R
for f in $MODFILES
do
  echo
  echo "-----------------------------------------------"
  echo "Training model $f..."
  echo "-----------------------------------------------"
  echo
  Rscript "$f"
done
