source("./indicatorPlots.r")
dailyAspects <- analyzeSecurity("LINK-USD")
ggplot(data=dailyAspects[origin == 'NNUR',]) +
  aes(y=orb, x=diffPercent, color=type) +
  geom_point() +
  facet_grid(aspect ~ ., scales="free_y") +
  scale_x_continuous(breaks=seq(-0.4, 0.4, by=0.1), limits=c(-0.4, 0.4)) +
  stat_ellipse(type="norm") +
  geom_smooth(orientation="y") +
  theme_black()
