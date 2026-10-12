# Soil nitrogen patterns with proximity to the plot edge

library(tidyverse)

input_file <- "outplut/combined_soil_leaf_with_myc_type_and_enrichment.csv"
figure_dir <- "outplut/figures"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

soil_data <- read_csv(input_file, show_col_types = FALSE) %>%
  filter(
    mycorrhizal_legacy %in% c("am", "ecm"),
    !is.na(distance_to_edge_m)
  ) %>%
  mutate(
    soil_cn_ratio = soil_percent_c / soil_percent_n,
    plot_type = factor(
      mycorrhizal_legacy,
      levels = c("am", "ecm"),
      labels = c("AM plots", "EcM plots")
    )
  )

plot_theme <- theme_classic(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "grey95", color = "grey75"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

plot_soil_n <- soil_data %>%
  filter(!is.na(soil_percent_n)) %>%
  ggplot(aes(x = distance_to_edge_m, y = soil_percent_n)) +
  geom_point(alpha = 0.55, size = 2, color = "#356A87") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#173F5F", fill = "#9CC4D8", linewidth = 1) +
  facet_wrap(~plot_type, nrow = 1) +
  labs(
    title = "Soil nitrogen along the plot-edge gradient",
    subtitle = "Lines show linear fits; shading shows 95% confidence intervals",
    x = "Distance from plot edge (m)",
    y = "Soil nitrogen (%N)"
  ) +
  plot_theme

plot_soil_d15n <- soil_data %>%
  filter(!is.na(soil_d15n)) %>%
  ggplot(aes(x = distance_to_edge_m, y = soil_d15n)) +
  geom_point(alpha = 0.55, size = 2, color = "#A65D2A") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#713B17", fill = "#E5B98F", linewidth = 1) +
  facet_wrap(~plot_type, nrow = 1) +
  labs(
    title = expression(paste("Soil ", delta^{15}, "N along the plot-edge gradient")),
    subtitle = "Lines show linear fits; shading shows 95% confidence intervals",
    x = "Distance from plot edge (m)",
    y = expression(paste("Soil ", delta^{15}, "N (\u2030)"))
  ) +
  plot_theme

ggsave(
  file.path(figure_dir, "soil_percent_n_vs_edge_by_plot_type.png"),
  plot_soil_n, width = 10, height = 5.5, dpi = 300, bg = "white"
)

ggsave(
  file.path(figure_dir, "soil_d15n_vs_edge_by_plot_type.png"),
  plot_soil_d15n, width = 10, height = 5.5, dpi = 300, bg = "white"
)

plot_soil_cn <- soil_data %>%
  filter(
    !is.na(soil_cn_ratio),
    is.finite(soil_cn_ratio),
    myc_type %in% c("am", "ecm")
  ) %>%
  ggplot(aes(
    x = distance_to_edge_m,
    y = soil_cn_ratio,
    color = myc_type,
    fill = myc_type
  )) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = TRUE,
    linewidth = 1, fullrange = FALSE
  ) +
  facet_wrap(~plot_type, nrow = 1) +
  scale_color_manual(
    values = c("am" = "#67ADD3", "ecm" = "#FF8737"),
    labels = c("am" = "AM", "ecm" = "EcM")
  ) +
  scale_fill_manual(
    values = c("am" = "#67ADD3", "ecm" = "#FF8737"),
    labels = c("am" = "AM", "ecm" = "EcM")
  ) +
  labs(
    title = "Soil C:N ratio along the plot-edge gradient",
    subtitle = "Lines show linear fits; shading shows 95% confidence intervals",
    x = "Distance from plot edge (m)",
    y = "Soil C:N ratio",
    color = "Mycorrhizal association",
    fill = "Mycorrhizal association"
  ) +
  plot_theme +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "soil_cn_ratio_vs_edge_by_plot_type.png"),
  plot_soil_cn, width = 10, height = 5.5, dpi = 300, bg = "white"
)

species_colors <- c(
  "ACRU" = "#1B9E77",
  "ACSA" = "#66A61E",
  "NYSY" = "#1F78B4",
  "PRSE" = "#6A3D9A",
  "BELE" = "#E6AB02",
  "CACO" = "#E7298A",
  "QURU" = "#D95F02",
  "TIAM" = "#A6761D"
)

plot_soil_d15n_species <- soil_data %>%
  filter(
    !is.na(soil_d15n),
    !is.na(species),
    species %in% names(species_colors)
  ) %>%
  ggplot(aes(
    x = distance_to_edge_m,
    y = soil_d15n,
    color = species,
    fill = species
  )) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    linewidth = 1, fullrange = FALSE
  ) +
  facet_wrap(~plot_type, nrow = 1) +
  scale_color_manual(values = species_colors, drop = FALSE) +
  scale_fill_manual(values = species_colors, drop = FALSE) +
  labs(
    title = expression(paste("Soil ", delta^{15}, "N along the plot-edge gradient")),
    subtitle = "Separate linear fits by tree species",
    x = "Distance from plot edge (m)",
    y = expression(paste("Soil ", delta^{15}, "N (\u2030)")),
    color = "Tree species",
    fill = "Tree species"
  ) +
  plot_theme +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    fill = "none"
  )

ggsave(
  file.path(figure_dir, "soil_d15n_vs_edge_by_species_and_plot_type.png"),
  plot_soil_d15n_species, width = 11, height = 6.5, dpi = 300, bg = "white"
)

plot_soil_d15n_myc <- soil_data %>%
  filter(
    !is.na(soil_d15n),
    myc_type %in% c("am", "ecm")
  ) %>%
  ggplot(aes(
    x = distance_to_edge_m,
    y = soil_d15n,
    color = myc_type,
    fill = myc_type
  )) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = TRUE,
    linewidth = 1, fullrange = FALSE
  ) +
  facet_wrap(~plot_type, nrow = 1) +
  scale_color_manual(
    values = c("am" = "#67ADD3", "ecm" = "#FF8737"),
    labels = c("am" = "AM", "ecm" = "EcM")
  ) +
  scale_fill_manual(
    values = c("am" = "#67ADD3", "ecm" = "#FF8737"),
    labels = c("am" = "AM", "ecm" = "EcM")
  ) +
  labs(
    title = expression(paste("Soil ", delta^{15}, "N along the plot-edge gradient")),
    subtitle = "Separate linear fits by mycorrhizal association; shading shows 95% confidence intervals",
    x = "Distance from plot edge (m)",
    y = expression(paste("Soil ", delta^{15}, "N (\u2030)")),
    color = "Mycorrhizal association",
    fill = "Mycorrhizal association"
  ) +
  plot_theme +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "soil_d15n_vs_edge_by_myc_and_plot_type.png"),
  plot_soil_d15n_myc, width = 10, height = 5.5, dpi = 300, bg = "white"
)
