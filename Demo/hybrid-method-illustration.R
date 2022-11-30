dta <- data.table(
  year = 1980:2020,
  y = 1
)

plt <- ggplot(dta, aes(year, y)) +
  geom_segment(aes(y = 0, yend = y, xend = year)) +
  coord_equal(clip = "off", xlim = c(1990, 2020)) +
  scale_y_continuous("", breaks = NULL, expand = expansion(0)) +
  scale_x_continuous(
    "Year", 
    breaks = 1983:2019 - 0.5,
    labels = function(x) x + 0.5,
    guide = guide_axis(angle = 45), 
    expand = expansion(add = 1)
  ) +
  ggthemes::theme_few(base_size = 15) +
  theme(
    plot.subtitle = element_text(margin = margin(0, 0, 100, 0))
  ) +
  labs(
    title = "5-year follow-up | Hybrid approach",
    subtitle = "Computing survival using hybrid approach for the year 2017"
  )

plt + annotate(
  geom = "text",
  label = "current year",
  x = 2019 - 0.5,
  y = 1,
  family = "mono",
  angle = 90,
  hjust = -0.05,
  vjust = 0,
  size = 6
) + annotate(
  geom = "text",
  label = "year",
  x = 2017 - 0.5,
  y = 1,
  family = "mono",
  angle = 90,
  hjust = -0.05,
  vjust = 0,
  size = 6
) + annotate(
  geom = "rect",
  xmin = 2019 - 5,
  xmax = 2019,
  ymin = 0,
  ymax = 1,
  fill = "forestgreen",
  alpha = 0.25
) + annotate(
  geom = "rect",
  xmin = 2017 - 5,
  xmax = 2017,
  ymin = 0,
  ymax = 1,
  fill = "royalblue",
  alpha = 0.25
)

