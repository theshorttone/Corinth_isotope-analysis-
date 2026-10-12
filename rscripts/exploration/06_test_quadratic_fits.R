# Exploratory quadratic fits: growth and foliar 15N vs. distance to edge
# Generated 2026-08-11

library(ggplot2)

input_file <- "outplut/alldata_03262026.csv"
output_dir <- "outplut/figures/quadratic_tests"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dat <- read.csv(input_file, check.names = FALSE)
dat <- subset(dat, year == 2023)
dat <- unique(dat) # source table contains each 2023 observation twice

format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  paste0("= ", formatC(p, format = "f", digits = 3))
}

fit_one <- function(response, response_label, title_label, file_stub) {
  d <- dat[, c("distance_to_edge_m", response)]
  names(d)[2] <- "response"
  d <- d[complete.cases(d), ]

  linear <- lm(response ~ distance_to_edge_m, data = d)
  quadratic <- lm(response ~ distance_to_edge_m + I(distance_to_edge_m^2), data = d)
  quad_summary <- summary(quadratic)
  quad_coef <- coef(quad_summary)["I(distance_to_edge_m^2)", ]
  overall_p <- pf(
    quad_summary$fstatistic[[1]],
    quad_summary$fstatistic[[2]],
    quad_summary$fstatistic[[3]],
    lower.tail = FALSE
  )
  comparison <- anova(linear, quadratic)
  added_curve_p <- comparison$`Pr(>F)`[2]

  annotation <- paste0(
    "Quadratic term p ", format_p(quad_coef[["Pr(>|t|)"]]),
    "\nOverall model p ", format_p(overall_p),
    "\nAdjusted R² = ", sprintf("%.3f", quad_summary$adj.r.squared),
    "   n = ", nrow(d)
  )

  p <- ggplot(d, aes(distance_to_edge_m, response)) +
    geom_point(alpha = 0.5, size = 2, color = "grey35") +
    geom_smooth(
      method = "lm", formula = y ~ x + I(x^2),
      se = TRUE, linewidth = 1.1, color = "#2166AC", fill = "#92C5DE"
    ) +
    annotate(
      "label", x = Inf, y = Inf, label = annotation,
      hjust = 1.05, vjust = 1.15, size = 3.8, linewidth = 0.25
    ) +
    labs(
      x = "Distance from forest edge (m)",
      y = response_label,
      title = paste(title_label, "vs. distance to edge"),
      subtitle = "Exploratory quadratic regression with 95% confidence interval"
    ) +
    theme_classic(base_size = 13) +
    theme(plot.title.position = "plot")

  figure_file <- file.path(output_dir, paste0(file_stub, ".png"))
  ggsave(figure_file, p, width = 7, height = 5.5, dpi = 300, bg = "white")

  stats <- data.frame(
    response = response,
    n = nrow(d),
    intercept = unname(coef(quadratic)[1]),
    linear_estimate = unname(coef(quadratic)[2]),
    linear_p_value = coef(quad_summary)["distance_to_edge_m", "Pr(>|t|)"],
    quadratic_estimate = unname(coef(quadratic)[3]),
    quadratic_p_value = quad_coef[["Pr(>|t|)"]],
    overall_model_p_value = overall_p,
    adjusted_r_squared = quad_summary$adj.r.squared,
    linear_aic = AIC(linear),
    quadratic_aic = AIC(quadratic),
    linear_vs_quadratic_p_value = added_curve_p,
    figure_file = figure_file
  )

  list(plot = p, stats = stats)
}

growth <- fit_one(
  "height_change", "Height change (cm)", "Height change (cm)",
  "test_quadratic_height_change_vs_distance_to_edge"
)

foliar_n15 <- fit_one(
  "foliar_15n_enrichment", expression(paste("Foliar ", delta^{15}, "N enrichment (\u2030)")),
  "Foliar δ15N enrichment (‰)",
  "test_quadratic_foliar_15n_vs_distance_to_edge"
)

stats <- rbind(growth$stats, foliar_n15$stats)
write.csv(
  stats,
  file.path(output_dir, "quadratic_fit_statistics.csv"),
  row.names = FALSE
)

# A two-panel copy is convenient for side-by-side assessment when patchwork is available.
if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- growth$plot + foliar_n15$plot + patchwork::plot_annotation(tag_levels = "A")
  ggsave(
    file.path(output_dir, "test_quadratic_growth_and_foliar_15n_combined.png"),
    combined, width = 12, height = 5.5, dpi = 300, bg = "white"
  )
}

print(stats)
