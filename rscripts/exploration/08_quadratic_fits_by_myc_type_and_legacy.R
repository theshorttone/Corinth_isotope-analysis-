# Exploratory quadratic fits split by mycorrhizal type and site legacy
# Generated 2026-08-11

library(ggplot2)

input_file <- "outplut/alldata_03262026.csv"
output_dir <- "outplut/figures/quadratic_tests"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dat <- read.csv(input_file, check.names = FALSE)
dat <- subset(dat, year == 2023 & !is.na(myc_type) & !is.na(mycorrhizal_legacy))
dat <- unique(dat) # source table contains each 2023 observation twice
dat$myc_type <- toupper(dat$myc_type)
dat$mycorrhizal_legacy <- toupper(dat$mycorrhizal_legacy)

format_p <- function(p) {
  if (is.na(p)) return(" = NA")
  if (p < 0.001) return(" < 0.001")
  paste0(" = ", sprintf("%.3f", p))
}

fit_groups <- function(response) {
  combinations <- unique(dat[, c("myc_type", "mycorrhizal_legacy")])
  combinations <- combinations[order(combinations$mycorrhizal_legacy, combinations$myc_type), ]

  do.call(rbind, lapply(seq_len(nrow(combinations)), function(i) {
    mt <- combinations$myc_type[i]
    lg <- combinations$mycorrhizal_legacy[i]
    d <- dat[dat$myc_type == mt & dat$mycorrhizal_legacy == lg,
             c("distance_to_edge_m", response)]
    names(d)[2] <- "response"
    d <- d[complete.cases(d), ]

    linear <- lm(response ~ distance_to_edge_m, data = d)
    quadratic <- lm(response ~ distance_to_edge_m + I(distance_to_edge_m^2), data = d)
    s <- summary(quadratic)
    overall_p <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)

    data.frame(
      response = response,
      myc_type = mt,
      mycorrhizal_legacy = lg,
      n = nrow(d),
      linear_estimate = unname(coef(quadratic)[2]),
      quadratic_estimate = unname(coef(quadratic)[3]),
      quadratic_p_value = coef(s)["I(distance_to_edge_m^2)", "Pr(>|t|)"],
      overall_model_p_value = overall_p,
      adjusted_r_squared = s$adj.r.squared,
      linear_aic = AIC(linear),
      quadratic_aic = AIC(quadratic),
      linear_vs_quadratic_p_value = anova(linear, quadratic)$`Pr(>F)`[2],
      x = min(d$distance_to_edge_m),
      y = max(d$response)
    )
  }))
}

make_plot <- function(response, y_label, title_label) {
  d <- dat[, c("distance_to_edge_m", response, "myc_type", "mycorrhizal_legacy")]
  names(d)[2] <- "response"
  d <- d[complete.cases(d), ]
  stats <- fit_groups(response)
  stats$label <- paste0(
    "quadratic p", vapply(stats$quadratic_p_value, format_p, character(1)),
    "\nmodel p", vapply(stats$overall_model_p_value, format_p, character(1)),
    "\nR²adj = ", sprintf("%.2f", stats$adjusted_r_squared), ", n = ", stats$n
  )

  p <- ggplot(d, aes(distance_to_edge_m, response, color = myc_type)) +
    geom_point(alpha = 0.55, size = 1.8, show.legend = FALSE) +
    geom_smooth(
      method = "lm", formula = y ~ x + I(x^2), se = TRUE,
      linewidth = 1, alpha = 0.2, show.legend = FALSE
    ) +
    geom_text(
      data = stats, aes(x = x, y = y, label = label),
      inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3.1
    ) +
    facet_grid(
      rows = vars(mycorrhizal_legacy), cols = vars(myc_type), scales = "free_y",
      labeller = labeller(
        myc_type = c(AM = "AM seedlings", ECM = "ECM seedlings"),
        mycorrhizal_legacy = c(AM = "AM legacy site", ECM = "ECM legacy site")
      )
    ) +
    scale_color_manual(values = c(AM = "#2166AC", ECM = "#D6604D")) +
    labs(
      x = "Distance from forest edge (m)", y = y_label,
      title = title_label,
      subtitle = "Quadratic regressions by seedling mycorrhizal type and site legacy (95% CI)"
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title.position = "plot",
      strip.background = element_rect(fill = "grey95", color = "grey75"),
      strip.text = element_text(face = "bold")
    )

  list(plot = p, stats = stats)
}

growth <- make_plot("height_change", "Height change (cm)", "Growth vs. distance to edge")
n15 <- make_plot(
  "foliar_15n_enrichment", "Foliar δ¹⁵N enrichment (‰)",
  "Foliar δ¹⁵N vs. distance to edge"
)

ggsave(file.path(output_dir, "test_quadratic_growth_by_myc_and_legacy.png"), growth$plot,
       width = 10, height = 8, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "test_quadratic_n15_by_myc_and_legacy.png"), n15$plot,
       width = 10, height = 8, dpi = 300, bg = "white")

all_stats <- rbind(growth$stats, n15$stats)
write.csv(all_stats, file.path(output_dir, "quadratic_fit_statistics_by_myc_and_legacy.csv"), row.names = FALSE)

if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- growth$plot / n15$plot + patchwork::plot_annotation(tag_levels = "A")
  ggsave(file.path(output_dir, "test_quadratic_by_myc_and_legacy_combined.png"), combined,
         width = 11, height = 17, dpi = 300, bg = "white")
}

print(all_stats[, c("response", "myc_type", "mycorrhizal_legacy", "n",
                    "quadratic_p_value", "overall_model_p_value", "adjusted_r_squared")])
