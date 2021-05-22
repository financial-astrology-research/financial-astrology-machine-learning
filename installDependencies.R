# Title     : Install project R package dependencies.
# Created by: pablocc
# Created on: 22/05/2021

# R4 core needs to be installed in the OS in order to have Rscript bin to run dependencies installation.

packagesList <- c(
  'GA',
  'ModelsMetrics',
  'R.cache',
  'SIT',
  'boot',
  'caret',
  'compiler',
  'cowplot',
  'curl',
  'data.table',
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

# You need to have installed "libcurl-devel" in your OS.
library(curl)
curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
install.packages('sit', repos = NULL, type='source')