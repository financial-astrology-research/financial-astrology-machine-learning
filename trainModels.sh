#!/usr/bin/env bash

MODFILES="*knnV*.R"

trap "exit" INT
for f in $MODFILES
do
  echo
  echo "-----------------------------------------------"
  echo "Training model $f..."
  echo "-----------------------------------------------"
  echo
  Rscript "$f"
done
