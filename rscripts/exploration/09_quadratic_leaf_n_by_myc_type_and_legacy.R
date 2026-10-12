# Exploratory quadratic fit for leaf percent N vs. distance to edge,
# split by seedling mycorrhizal type and site legacy.

library(ggplot2)

input_file <- "outplut/alldata_03262026.csv"
output_dir <- "outplut/figures/quadratic_tests"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dat <- read.csv(input_file, check.names = FALSE)
dat <- subset(
  dat,
  year == 2023 & !is.na(myc_type) & !is.na(mycorrhizal_legacy) &
    !is.na(distance_to_edge_m) & !is.na(leaf_percent_n)
)
dat <- unique(dat) # source table contains each 2023 observation twice
dat$myc_type <- toupper(dat$myc_type)
dat$mycorrhizal_legacy <- toupper(dat$mycorrhizal_legacy)

format_p <- function(p) {
  if (is.na(p)) return(" = NA")
  if (p < 0.001) return(" < 0.001")
  paste0(" = ", sprintf("%.3f", p))
}

combinations <- unique(dat[, c("myc_type", "mycorrhizal_legacy")])
combinations <- combinations[order(combinations$mycorrhizal_legacy, combinations$myc_type), ]

stats <- do.call(rbind, lapply(seq_len(nrow(combinations)), function(i) {
  mt <- combinations$myc_type[i]
  lg <- combinations$mycorrhizal_legacy[i]
  d <- subset(dat, myc_type == mt & mycorrhizal_legacy == lg)

  linear <- lm(leaf_percent_n ~ distance_to_edge_m, data = d)
  quadratic <- lm(
    leaf_percent_n ~ distance_to_edge_m + I(distance_to_edge_m^2), data = d
  )
  s <- summary(quadratic)
  overall_p <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)

  data.frame(
    response = "leaf_percent_n",
    myc_type = mt,
    mycorrhizal_legacy = lg,
    n = nrow(d),
    intercept = unname(coef(quadratic)[1]),
    linear_estimate = unname(coef(quadratic)[2]),
    linear_p_value = coef(s)["distance_to_edge_m", "Pr(>|t|)"],
    quadratic_estimate = unname(coef(quadratic)[3]),
    quadratic_p_value = coef(s)["I(distance_to_edge_m^2)", "Pr(>|t|)"],
    overall_model_p_value = overall_p,
    adjusted_r_squared = s$adj.r.squared,
    linear_aic = AIC(linear),
    quadratic_aic = AIC(quadratic),
    linear_vs_quadratic_p_value = anova(linear, quadratic)$`Pr(>F)`[2],
    x = min(d$distance_to_edge_m),
    y = max(d$leaf_percent_n)
  )
}))

stats$label <- paste0(
  "quadratic p", vapply(stats$quadratic_p_value, format_p, character(1)),
  "\nmodel p", vapply(stats$overall_model_p_value, format_p, character(1)),
  "\nR²adj = ", sprintf("%.2f", stats$adjusted_r_squared), ", n = ", stats$n
)

leaf_n_plot <- ggplot(dat, aes(distance_to_edge_m, leaf_percent_n, color = myc_type)) +
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
    x = "Distance from forest edge (m)",
    y = "Leaf N (%)",
    title = "Leaf N (%) vs. distance to edge",
    subtitle = "Quadratic regressions by seedling mycorrhizal type and site legacy (95% CI)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title.position = "plot",
    strip.background = element_rect(fill = "grey95", color = "grey75"),
    strip.text = element_text(face = "bold")
  )

figure_file <- file.path(output_dir, "test_quadratic_leaf_n_by_myc_and_legacy.png")
stats_file <- file.path(output_dir, "quadratic_leaf_n_statistics_by_myc_and_legacy.csv")

ggsave(figure_file, leaf_n_plot, width = 10, height = 8, dpi = 300, bg = "white")
write.csv(stats[, !names(stats) %in% c("x", "y", "label")], stats_file, row.names = FALSE)

print(stats[, c("myc_type", "mycorrhizal_legacy", "n", "quadratic_p_value",
                "overall_model_p_value", "adjusted_r_squared")])
