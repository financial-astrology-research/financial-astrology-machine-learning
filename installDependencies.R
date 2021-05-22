# Title     : Install project R package dependencies.
# Created by: pablocc
# Created on: 22/05/2021

# R4 core needs to be installed in the OS in order to have Rscript bin to run dependencies installation.

packagesList <- c(
  'GA',
  'ModelsMetrics',
  'R.cache',
  'boot',
  'caret',
  'compiler',
  'cowplot',
  'curl',
  'data.table',
  'devtools',
  'gbm',
  'ggplot2',
  'grid',
  'microbenchmark',
  'paramtest',
  'psych',
  'purrr',
  'quantmod',
  'reshape2',
  'splus2R',
  'stringr',
  'zeallot'
)

install.packages(
  packagesList,
  repos="http://cran.wustl.edu/"
)
