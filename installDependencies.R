# Title     : Install project R package dependencies.
# Created by: pablocc
# Created on: 22/05/2021

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

library(curl)
curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
install.packages('sit', repos = NULL, type='source')