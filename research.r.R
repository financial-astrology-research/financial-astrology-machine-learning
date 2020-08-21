source("./indicatorPlots.r")
dailyAspects <- predictSecurityModelA("EOS-USD")
dailyAspects[, diffPercent := round(diffPercent * 100, 1)]
dailyAspectsFiltered <- dailyAspects[
  p.x %ni% c('CE', 'JU', 'SA', 'UR', 'NE', 'PL')
][
  orb <= 2,
][
  aspect != '30'
][
  origin %ni% c('MESU')
]

ggplot(data=dailyAspectsFiltered) +
  aes(y=sp.y, x=diffPercent) +
  #aes(y=spn.x, x=diffPercent, color=type) +
  geom_point(color="white") +
  facet_grid(aspect ~ origin, scales="free_y") +
  stat_ellipse(type="norm", color="yellow") +
  scale_x_continuous(limits=c(-10, 10)) +
  #scale_y_continuous(limits=c(0, 1)) +
  #geom_smooth(orientation="y") +
  theme_black()


# CONCLUSIONS (DAILY CURRENT ASPECTS):
# 1) The retrograde motions of ME highly correlates with positive/negative aspect effect.
# 2) With lower evidence the MA, CE and JU retrograde motion seems that may affect the effect but we don't have enough data.
# 3) The applicative / separative aspect transition show an important change of the effect after partil.
# 4) The classical aspects in partil orb seems to show the highest effect.
# 5) The interaction of other aspects within same orb correlates with higher effect, joining forces.

