source("./indicatorPlots.r")
dailyAspects <- predictSecurityModelA("EOS-USD")
dailyAspects[, diffPercent := round(diffPercent * 100, 1)]
dailyAspectsFast <- dailyAspects[
  p.x %ni% c('CE', 'JU', 'SA', 'UR', 'NE', 'PL')
][
  orb <= 2,
][
  origin %ni% c('MESU')
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

ggplot(data = dailyAspectsFast) +
  aes(y = sp.y, x = diffPercent) +
  geom_point(color = "white") +
  facet_grid(aspect ~ origin, scales = "free_y") +
  stat_ellipse(type = "norm", color = "yellow") +
  scale_x_continuous(limits = c(-10, 10)) +
  theme_black()

# Slow planets former bodies speed effect.
dailyAspectsSlow <- dailyAspects[
  p.x %in% c('CE', 'JU', 'SA', 'UR', 'NE', 'PL')
][
  orb <= 2,
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

# Cumulative former bodies external aspects energy effect.
ggplot(data = dailyAspectsFast) +
  aes(y = entot, x = abs(diffPercent)) +
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

