# Exploratory quadratic fits by mycorrhizal type
# Generated 2026-08-11

library(ggplot2)

input_file <- "outplut/alldata_03262026.csv"
output_dir <- "outplut/figures/quadratic_tests"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dat <- read.csv(input_file, check.names = FALSE)
dat <- subset(dat, year == 2023)
dat <- unique(dat) # source table contains each 2023 observation twice
dat$species <- toupper(dat$species)
dat$myc_type <- toupper(dat$myc_type)

format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return(" < 0.001")
  paste0(" = ", sprintf("%.3f", p))
}

group_stats <- function(data, response, group_var) {
  groups <- unique(data[[group_var]])
  groups <- sort(groups[!is.na(groups) & groups != ""])

  rows <- lapply(groups, function(g) {
    d <- data[data[[group_var]] == g, c("distance_to_edge_m", response)]
    names(d)[2] <- "response"
    d <- d[complete.cases(d), ]
    if (nrow(d) < 5 || length(unique(d$distance_to_edge_m)) < 3) return(NULL)

    linear <- lm(response ~ distance_to_edge_m, data = d)
    quadratic <- lm(response ~ distance_to_edge_m + I(distance_to_edge_m^2), data = d)
    s <- summary(quadratic)
    overall_p <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
    quad_p <- coef(s)["I(distance_to_edge_m^2)", "Pr(>|t|)"]

    out <- data.frame(
      response = response,
      grouping = group_var,
      group = g,
      n = nrow(d),
      linear_estimate = unname(coef(quadratic)[2]),
      quadratic_estimate = unname(coef(quadratic)[3]),
      quadratic_p_value = quad_p,
      overall_model_p_value = overall_p,
      adjusted_r_squared = s$adj.r.squared,
      linear_aic = AIC(linear),
      quadratic_aic = AIC(quadratic),
      linear_vs_quadratic_p_value = anova(linear, quadratic)$`Pr(>F)`[2]
    )
    out[[group_var]] <- g
    out
  })
  do.call(rbind, rows)
}

make_group_plot <- function(response, y_label, title_label, group_var, palette = NULL) {
  d <- dat[, c("distance_to_edge_m", response, group_var)]
  names(d)[2] <- "response"
  d <- d[complete.cases(d), ]
  stats <- group_stats(dat, response, group_var)

  ranges <- do.call(rbind, lapply(unique(d[[group_var]]), function(g) {
    z <- d[d[[group_var]] == g, ]
    out <- data.frame(x = min(z$distance_to_edge_m), y = max(z$response))
    out[[group_var]] <- g
    out
  }))
  annotation <- merge(stats, ranges, by = group_var)
  annotation$label <- paste0(
    "quadratic p", vapply(annotation$quadratic_p_value, format_p, character(1)),
    "\nmodel p", vapply(annotation$overall_model_p_value, format_p, character(1)),
    "\nR²adj=", sprintf("%.2f", annotation$adjusted_r_squared),
    ", n=", annotation$n
  )

  p <- ggplot(d, aes(distance_to_edge_m, response, color = .data[[group_var]])) +
    geom_point(alpha = 0.5, size = 1.7, show.legend = FALSE) +
    geom_smooth(
      method = "lm", formula = y ~ x + I(x^2), se = TRUE,
      linewidth = 1, alpha = 0.2, show.legend = FALSE
    ) +
    geom_text(
      data = annotation,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3
    ) +
    facet_wrap(stats::as.formula(paste("~", group_var)), scales = "free_y") +
    labs(
      x = "Distance from forest edge (m)", y = y_label,
      title = paste(title_label, "by", ifelse(group_var == "myc_type", "mycorrhizal type", "tree species")),
      subtitle = "Quadratic regressions with 95% confidence intervals"
    ) +
    theme_classic(base_size = 12) +
    theme(plot.title.position = "plot", strip.background = element_blank(), strip.text = element_text(face = "bold"))

  if (!is.null(palette)) p <- p + scale_color_manual(values = palette)
  list(plot = p, stats = stats)
}

myc_palette <- c("AM" = "#2166AC", "ECM" = "#D6604D")
results <- list(
  growth_myc = make_group_plot("height_change", "Height change (cm)", "Growth vs. distance to edge", "myc_type", myc_palette),
  n15_myc = make_group_plot("foliar_15n_enrichment", "Foliar δ¹⁵N enrichment (‰)", "Foliar δ¹⁵N vs. distance to edge", "myc_type", myc_palette)
)

sizes <- list(growth_myc = c(9, 5), n15_myc = c(9, 5))
for (nm in names(results)) {
  ggsave(
    file.path(output_dir, paste0("test_quadratic_", nm, ".png")),
    results[[nm]]$plot, width = sizes[[nm]][1], height = sizes[[nm]][2], dpi = 300, bg = "white"
  )
}

all_stats <- do.call(rbind, lapply(results, function(x) x$stats))
write.csv(all_stats, file.path(output_dir, "quadratic_fit_statistics_by_myc_type.csv"), row.names = FALSE)

if (requireNamespace("patchwork", quietly = TRUE)) {
  myc_combined <- results$growth_myc$plot / results$n15_myc$plot + patchwork::plot_annotation(tag_levels = "A")
  ggsave(file.path(output_dir, "test_quadratic_by_myc_combined.png"), myc_combined, width = 10, height = 10, dpi = 300, bg = "white")
}

print(all_stats[, c("response", "grouping", "group", "n", "quadratic_p_value", "overall_model_p_value", "adjusted_r_squared")])
