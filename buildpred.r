# Model 143
system2('git', 'checkout az30-ds5-t20-sma-KM2.7.12-pSUMOMEVEMAJUNNSA-composite-pc9-pm1-el100-cm1-rwsel-pop1000', stdout=T)
source("~/trading/analysis.r")
allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
removed <- file.remove(allFiles)
source("~/trading/benchmarks/benchmark_143.txt")

# Model 148
system2('git', 'checkout az30-ds5-t20-sma-KM2.7.12-pSUMOMEVEMAJUNNSA-composite-pc9-pm1-el100-cm1-rwsel-pop1000', stdout=T)
source("~/trading/analysis.r")
allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
removed <- file.remove(allFiles)
source("~/trading/benchmarks/benchmark_148.txt")

# Model 165
system2('git', 'checkout topnsp9-azsp30-ds5_6-sma-all2.5.7.12-pSUMOMEVEMAJUNNSA-pc9-pm1-el100-cm1-rwsel-pop1000', stdout=T)
source("~/trading/analysis.r")
allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
removed <- file.remove(allFiles)
source("~/trading/benchmarks/benchmark_165.txt")

# Model 166
system2('git', 'checkout topnsp10-azsp30-ds5_6-sma-all2.5.7.12-conp-pSUMOMEVEMAJUNNSA-pc9-pm1-el100-cm1-rwsel-pop1000', stdout=T)
source("~/trading/analysis.r")
allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
removed <- file.remove(allFiles)
source("~/trading/benchmarks/benchmark_166.txt")

# Model 175
system2('git', 'checkout topnsp16-azsp30-ds5_6-sma-psm20-all2.5.7.12-conp-pSUMOMEVEMAJUNNSA-pc9-pm1-el100-cm1-rwsel-pop1000-rcv', stdout=T)
source("~/trading/analysis.r")
allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
removed <- file.remove(allFiles)
source("~/trading/benchmarks/benchmark_175.txt")

# Model 179
system2('git', 'checkout topnsp19-azsp30-ds5_6-sma-psm20-Modern3.6.10-pSUMOMEVEMAJUNNSA-pc9-pm1-el100-cm1-rwsel-pop1000-rcv', stdout=T)
source("~/trading/analysis.r")
allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
removed <- file.remove(allFiles)
source("~/trading/benchmarks/b179_stocks.txt")

# Model 183
system2('git', 'checkout topnsp23-azsp30-ds5_6-sma-psm10-all2.5.7.12-pSUMOMEVEMAJUNNSA-pc9-pm1-el100-cm1-rwsel-pop1000-rcv', stdout=T)
source("~/trading/analysis.r")
allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
removed <- file.remove(allFiles)
source("~/trading/benchmarks/b183_stocks.txt")

# Model 197
system2('git', 'checkout tnsp37-azsp30-ds5_6-sma-psm10-all2.5.7.12-pSUMOMEVEMAJUNNSA-gac9m1e100cm1rw-1000-60-cv2', stdout=T)
source("~/trading/analysis.r")
allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
removed <- file.remove(allFiles)
source("~/trading/benchmarks/b197_stocks.txt")

