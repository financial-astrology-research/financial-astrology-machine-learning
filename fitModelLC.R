# Title     : Single row price/aspects per day modeling.
# Created by: pablocc
# Created on: 28/09/2020

library(caret)
library(magrittr)
library(psych)
library(plyr)
library(rattle)
library(gvlma)
library(arm)
library(glmulti)
source("./analysis.r")
source("./indicatorPlots.r")

dailyAspectPlanetCumulativeEnergy <- prepareHourlyAspectsModelLC()

dailyAspectPlanetCumulativeEnergy[,
  sell := a180_SU
    + a0_VE
    + a45_NN
    + a90_PL
    + a60_NN
    + a120_SU
    + a45_NE
    + a45_UR
    + a90_SU
    + a180_VE
    + a0_NE
    + a30_NN
    + a30_SU
    + a120_JU
    + a30_MA
    + a30_NE
    + a45_JU
    + a180_ME
]

dailyAspectPlanetCumulativeEnergy[,
  sell2 := a0_MO
    + a90_ME
    + a90_MO
    + a45_ME
    + a120_MA
    + a120_MO
]

dailyAspectPlanetCumulativeEnergy[,
  buy := a60_NE
    + a150_VE
    + a150_NN
    + a60_SU
    + a45_SA
    + a30_PL
    + a180_SA
    + a150_SU
    + a120_NE
    + a90_NN
    + a0_SA
    + a0_NN
    + a90_VE
    + a0_MA
    + a150_MA
    + a120_NN
    + a90_UR
    + a90_NE
    + a45_SU
    + a45_PL
    + a0_PL
]

dailyAspectPlanetCumulativeEnergy[,
  buy2 := a30_JU
    + a180_NN
    + a150_MO
    + a60_SA
    + a90_JU
]

dailyAspectPlanetCumulativeEnergy[,
  buypow := buy - sell
]

dailyAspectPlanetCumulativeEnergy[,
  buypow2 := buy2 - sell2
]

dailyAspectPlanetCumulativeEnergy[,
  sellpow := sell - buy
]

dailyAspectPlanetCumulativeEnergy[,
  sellpow2 := sell2 - buy2
]

symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 14, 28, "%Y-%m-%d",
  "2010-01-01", "2020-06-30"
)

aspectView <- merge( securityData[, c('Date', 'diffPercent')],
  dailyAspectPlanetCumulativeEnergy, by = "Date"
)

#aspectView[, diffPercent := abs(diffPercent)]

varCorrelations <- aspectView[, -c('Date')] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])
print(finalCorrelations)

#selectCols <- c("Date", "buy", "sell", "buy2", "sell2", "buypow", "sellpow")
selectCols <- names(aspectView)[c(1, seq(3, 17), seq(80, 90))]
modelSearch <- glmulti(
  y = "diffPercent",
  xr = selectCols[-1],
  data = aspectView,
  #exclude=c("sp.y", "sp.x", "dc.x", "dc.y"),
  #minsize = 15,
  level = 1, marginality = F, intercept = T, crit = "aicc",
  method = "g", plotty = F,
  popsize = 300
  #mutrate = 0.01, sexrate = 0.1, imm = 0.1,
)

plot(modelSearch, type = "s")
print(modelSearch@objects[[1]]$formula)

# Review the best fit.
modelFit <- lm(
  modelSearch@objects[[1]]$formula,
  data = aspectView
)

modelFit %>% summary()
modelFit %>% plot()
modelFit %>% coefplot()

# Validate with reserved data.
securityDataTest <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2020-07-01")
aspectViewValidate <- dailyAspectPlanetCumulativeEnergy[, ..selectCols]
aspectViewValidate$diffPredict <- predict(modelFit, aspectViewValidate)
aspectViewValidate$diffPredictSmooth <- SMA(aspectViewValidate$diffPredict, 7)
# Dsiplay projected prediction in chart
ggplot(data = aspectViewValidate[Date >= Sys.Date() - 60,]) +
  geom_line(aes(x = Date, y = diffPredictSmooth), colour = "black", alpha = 0.7) +
  scale_x_date(date_breaks = "7 days", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, size = 10), axis.title.x = element_blank(), axis.title.y = element_blank())
aspectViewValidate <- merge(securityDataTest[, c('Date', 'diffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate[, c('Date', 'diffPercent', 'diffPredict')]
plot(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict)
cor(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict) %>% print()
with(aspectViewValidate, mean((diffPercent - diffPredict)^2)) %>% sqrt()
#plot(aspectViewValidate$a180_SU, type = "l")
#fwrite(aspectView, paste("~/Desktop/", symbol, "cumenergy.csv", sep = "-"))
