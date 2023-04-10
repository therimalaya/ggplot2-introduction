library(ggplot2)
library(data.table)
library(survival)
library(survminer)

data(Melanoma, package = "MASS")
setDT(Melanoma)

Melanoma[, status_all := status %in% c(1, 3)]
Melanoma[, tstage := fcase(
  thickness <= 1, "T1",
  thickness > 1 & thickness <= 2, "T2",
  thickness > 2 & thickness <= 4, "T3",
  default = "T4"
)]

mdl <- survfit(Surv(time, status_all) ~ tstage, data = Melanoma)
surv_plot <- ggsurvplot(
  fit = mdl, 
  risk.table = TRUE,
  palette = "Set1",
  tables.height = 0.3,
  ggtheme = ggthemes::theme_few(),
  tables.theme = ggthemes::theme_few()
)
surv_plot$plot <- surv_plot$plot +
  scale_x_continuous(
    name = "Time (in years)",
    breaks = seq(0, 15, 0.5) * 365.241,
    labels = function(x) x / 365.241
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = function(x) paste(round(x * 100), "%")
  ) +
  theme(panel.grid = element_line(color = "#f0f0f0")) +
  scale_color_brewer(
    palette = "Set1",
    labels = paste0("T", 1:4)
  )

surv_plot$table <- surv_plot$table +
  scale_y_discrete(
    labels = function(x) gsub("tstage=", "", x)
  ) +
  scale_x_continuous(
    name = "Time (in years)",
    breaks = seq(0, 15, 0.5) * 365.241,
    labels = function(x) x / 365.241
  ) +
  theme(
    axis.text.y.left = element_text(size = 12)
  )

surv_plot


