library(psych)
library(magrittr)
source("./indicatorPlots.r")

symbol <- "EOS-USD"
securityData <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")

# Experiment grid search with different aspects energy factors.
filterFeatures <- c(
  'Date', 'origin', 'type', 'p.x', 'p.y',
  'orb', 'lon.y', 'lon.x',
  #'sp.y', 'sp.x', 'spn.y', 'spn.x',
  #'act', 'agt',
  paste("a", aspects, ".x", sep = ""),
  #paste("a", aspects, ".y", sep = ""),
  paste("a", aspects, sep = ""),
  paste("a", aspects, ".g", sep = ""),
  'orbdir'
)

dailyAspects <- prepareHourlyAspectsModelK()
dailyAspectsPrice <- merge(securityData[, c('Date', 'diffPercent')], dailyAspects, by = "Date")
dailyAspectsPrice[, result := cut(diffPercent, c(-100, 0, 100), c("down", "up"))]
#dailyAspects[, apos := a60.x + a60.y + a120.x + a120.y]
#dailyAspects[, aneg := a90.x + a90.y + a150.x + a150.y]
#dailyAspects[, aneg := a90.x + a90.y + a180.x + a180.y]
#dailyAspects[, apos := a60.x + a60.y + a120.x + a120.y]
#dailyAspects[, apos := a60.t + a60.t + a120.t + a120.t]
#dailyAspects[, aneg := a90.t + a90.t + a150.t + a150.t]
dailyAspectsPrice[, apos := a30.t + a60.t + a120.t]
dailyAspectsPrice[, aneg := a45.t + a90.t + a135.t]
dailyAspectsPrice[, adiff := apos - aneg]
# dailyAspects[, adiff := apos - aneg]
# dailyAspects[, apos2 := apos]
# dailyAspects[adiff > 0, apos2 := apos + a0 + a180 + a150]
# dailyAspects[, aneg2 := aneg]
# dailyAspects[adiff < 0, aneg2 := aneg + a0 + a180 + a150]
# dailyAspects[, adiff2 := apos2 - aneg2]
# dailyAspects[, orbtype := cut(orb, seq(0, 12, by = 1))]

# Price diff aspect histogram / x & y speed for fast planets except MO.
ggplot(data = dailyAspectsPrice[p.x %ni% c('MO', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL'),]) +
  geom_point(aes(y = spn.y, x = diffPercent), color = "gray") +
  stat_ellipse(aes(y = spn.y, x = diffPercent), type = "norm", color = "yellow") +
  facet_grid(aspect ~ origin, scales = "free") +
  theme_black()

ggplot(data = dailyAspectsPrice[p.x %ni% c('MO', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL'),]) +
  geom_point(aes(y = spn.x, x = diffPercent), color = "gray") +
  stat_ellipse(aes(y = spn.x, x = diffPercent), type = "norm", color = "yellow") +
  facet_grid(aspect ~ origin, scales = "free") +
  theme_black()

ggplot(data = dailyAspectsPrice[p.x %ni% c('MO', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL'),]) +
  aes(x = diffPercent) +
  geom_histogram(color = "gray", bins = 25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.6, alpha = 0.7) +
  facet_grid(aspect ~ origin, scales = "free_y") +
  theme_black()

# Price diff aspect histogram / x & y speed for MO and slow planets.
ggplot(data = dailyAspectsPrice[p.x %in% c('MO', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL'),]) +
  aes(x = diffPercent) +
  geom_histogram(color = "gray", bins = 25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.6, alpha = 0.7) +
  facet_grid(aspect ~ origin, scales = "free_y") +
  theme_black()

ggplot(data = dailyAspectsPrice[p.x %in% c('MO', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL'),]) +
  geom_point(aes(y = spn.y, x = diffPercent), color = "gray") +
  stat_ellipse(aes(y = spn.y, x = diffPercent), type = "norm", color = "yellow") +
  facet_grid(aspect ~ origin, scales = "free") +
  theme_black()

ggplot(data = dailyAspectsPrice[p.x %in% c('MO', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL'),]) +
  geom_point(aes(y = spn.x, x = diffPercent), color = "gray") +
  stat_ellipse(aes(y = spn.x, x = diffPercent), type = "norm", color = "yellow") +
  facet_grid(aspect ~ origin, scales = "free") +
  theme_black()

# Price diff to pos/neg momentum.
ggplot(data = dailyAspectsPrice[p.x %ni% c('MO', 'ME', 'NN', 'SA', 'UR', 'NE', 'PL'),]) +
  geom_point(aes(y = aneg, x = diffPercent, alpha = 0.5), color = "gray") +
  stat_ellipse(aes(y = aneg, x = diffPercent, alpha = 0.5), type = "norm", color = "yellow") +
  geom_point(aes(y = apos, x = diffPercent, alpha = 0.5), color = "pink") +
  stat_ellipse(aes(y = apos, x = diffPercent, alpha = 0.5), type = "norm", color = "pink") +
  scale_x_continuous(limits = c(-10, 10)) +
  facet_grid(aspect ~ origin, scales = "free_y") +
  #scale_y_continuous(limits=c(0, 1)) +
  #geom_smooth(orientation="y") +
  theme_black()

dailyAspectsFast <- dailyAspectsPrice[
  p.x %ni% c('CE', 'JU', 'SA', 'UR', 'NE', 'PL')
][
  origin %ni% c('MESU'),
]

# Fast planets former bodies speed effect.
ggplot(data = dailyAspectsFast) +
  aes(y = sp.x, x = diffPercent) +
  #aes(y=spn.x, x=diffPercent, color=type) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(-10, 10)) +
  #scale_y_continuous(limits=c(0, 1)) +
  #geom_smooth(orientation="y") +
  theme_black()

# Fast planets former bodies speed effect.
ggplot(data = dailyAspectsFast) +
  aes(y = sp.x, x = diffPercent) +
  #aes(y=spn.x, x=diffPercent, color=type) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(-10, 10)) +
  #scale_y_continuous(limits=c(0, 1)) +
  #geom_smooth(orientation="y") +
  theme_black()

ggplot(data = dailyAspectsFast) +
  aes(y = sp.y, x = diffPercent) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(-10, 10)) +
  theme_black()

# Cumulative former bodies external aspects energy effect.
ggplot(data = dailyAspectsFast) +
  aes(y = enpos, x = abs(diffPercent)) +
  #aes(y=spn.x, x=diffPercent, color=type) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(0, 10)) +
  #scale_y_continuous(limits=c(0, 1)) +
  #geom_smooth(orientation="y") +
  theme_black()

# Cumulative former bodies slow boody external energy.
ggplot(data = dailyAspectsFast) +
  aes(y = encum.y, x = abs(diffPercent)) +
  #aes(y=spn.x, x=diffPercent, color=type) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(0, 10)) +
  #scale_y_continuous(limits=c(0, 1)) +
  #geom_smooth(orientation="y") +
  theme_black()

# Cumulative former bodies fast boody external energy.
ggplot(data = dailyAspectsFast) +
  aes(y = encum.x, x = abs(diffPercent)) +
  #aes(y=spn.x, x=diffPercent, color=type) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(0, 10)) +
  #scale_y_continuous(limits=c(0, 1)) +
  #geom_smooth(orientation="y") +
  theme_black()

# Slow planets former bodies speed effect.
dailyAspectsSlow <- dailyAspectsPrice[
  p.x %in% c('CE', 'JU', 'SA', 'UR', 'NE', 'PL')
][
  origin %ni% c('MESU')
]

ggplot(data = dailyAspectsSlow) +
  aes(y = sp.x, x = diffPercent) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(-10, 10)) +
  theme_black()

ggplot(data = dailyAspectsSlow) +
  aes(y = sp.y, x = diffPercent) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(-10, 10)) +
  theme_black()


# Own aspect energy.
ggplot(data = dailyAspectsFast) +
  aes(y = ennow, x = abs(diffPercent)) +
  #aes(y=spn.x, x=diffPercent, color=type) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(0, 10)) +
  #scale_y_continuous(limits=c(0, 1)) +
  #geom_smooth(orientation="y") +
  theme_black()

# CONCLUSIONS (DAILY CURRENT ASPECTS):
# 1) The retrograde motions of ME highly correlates with positive/negative aspect effect.
# 2) With lower evidence the MA, CE and JU retrograde motion seems that may affect the effect but we don't have enough data.
# 3) Applicative / separative aspect transition show an important change of the effect after partil.
# 4) Classical aspects in partil orb seems to show the highest effect.
# 5) Interaction of other aspects within same orb correlates with higher effect, joining forces.
# 6) Aspects former planets received cumulative energy from other aspects show relation to the increase on priece effect
#    in few aspects, for higher cumulative energy more drastical price moves.
# 7) Some aspects with high cumulative energy activation seems to block price move effect that resume when effect pass away,
#    is possible that this is caused by the opposite polarity combination of former planets or external aspects that neutralize.

library(caret)
library(rattle)
library(tidyverse)
aspectView <- dailyAspectsPrice[p.x == "MO" & aspect == 0]
aspectView[, result := cut(diffPercent, c(-1, 0, 1), c("down", "up"))]
selectCols <- c("spd", "spp", "spr", "dcd", "dcp", "dcr", "VE", "VE.x", "acx", "acy", "result")
#selectCols <- c("dcd", "dcp", "dcr", "result")
#selectCols <- c(20, 22, 81:91, 92)
aspectView <- aspectView[, ..selectCols]
aspectViewTrain <- aspectView %>% sample_frac(.70)
aspectViewTest <- aspectView %>% anti_join(aspectViewTrain)

tree1 = train(
  result ~ .,
  data = aspectViewTrain,
  method = "rpart",
  trControl = trainControl(method = "cv")
)

summary(tree1$finalModel)

effect_p <- tree1 %>% predict(newdata = aspectViewTrain)
# Prediction results on train.
table(
  actualclass = aspectViewTrain$result,
  predictedclass = effect_p
) %>% confusionMatrix() %>% print()

effect_p <- tree1 %>% predict(newdata = aspectViewTest)
# Prediction results on test.
table(
  actualclass = aspectViewTest$result,
  predictedclass = effect_p
) %>% confusionMatrix() %>% print()

futureAspectsView <- dailyAspects[p.x == "MO" & aspect == 0 & Date >= as.Date("2020-08-01")]
selectCols <- c("spd", "spp", "spr", "dcd", "dcp", "dcr", "VE", "VE.x", "acx", "acy")
futureAspectsFeatures <- futureAspectsView[, ..selectCols]
effect_p <- tree1 %>% predict(newdata = futureAspectsFeatures)
futureAspectsView$effect_p <- effect_p

