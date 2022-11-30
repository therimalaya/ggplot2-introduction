library(data.table)
library(ggplot2)

expr_data <- ggpubr::diff_express
setDT(expr_data, keep.rownames = "rn")

fold_change <- 1.5
signif <- 0.05

expr_data[, sig_expr := padj <= signif & detection_call == 1]
expr_data <- expr_data[, group := fcase(
  sig_expr & abs(log2FoldChange) >= fold_change & log2FoldChange > 0, "up",
  sig_expr & abs(log2FoldChange) >= fold_change & log2FoldChange < 0, "down",
  default = NA_character_
)][!is.na(padj) & name != ""]

expr_data[, log2BaseMean := log2(baseMean + 1)]
lbl <- expr_data[, .N, by = group][, glue::glue_data(.SD, "{group} ({N})")]
top_name <- expr_data[!is.na(group)][order(padj), head(.SD, 5), by = group]

ggplot(expr_data, aes(log2BaseMean, log2FoldChange, color = group)) +
  geom_point(size = 0.5, alpha = 0.5) +
  ggrepel::geom_text_repel(
    data = top_name, aes(label = name), fontface = "bold", 
    arrow = arrow(length = unit(0.005, "npc")),
    nudge_x = 0.5, nudge_y = 0.5
  ) +
  geom_hline(yintercept = c(-1 * fold_change, fold_change), linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_color_brewer(palette = "Set1", na.value = "grey", labels = lbl) +
  theme_bw() +
  labs(x = "log2(Base Mean)", y = "log2(Fold Change)", color = "Group") +
  theme(
    legend.position = "bottom"
  )
