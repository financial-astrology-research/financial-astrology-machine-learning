#!/usr/bin/env bash

MODSGEN=$1
MODFILES="*knn$MODSGEN*.R"
echo "Models match pattern: $MODFILES"
echo

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
