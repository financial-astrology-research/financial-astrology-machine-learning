# Title     : Install project R package dependencies.
# Created by: pablocc
# Created on: 22/05/2021

# R4 core needs to be installed in the OS in order to have Rscript bin to run dependencies installation.

packagesList <- c(
  'GA',
  'ModelsMetrics',
  'R.cache',
  'bit64',
  'boot',
  'caret',
  'compiler',
  'data.table',
  'gbm',
  'ggplot2',
  'grid',
  'kknn',
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
