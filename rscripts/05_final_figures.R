# Final manuscript figures ------------------------------------------------
# Figure 2: distance-to-edge responses
# Figure S1: soil N and C:N raw data plus legacy means and 95% CIs

library(tidyverse)
library(lme4)
library(lmerTest) # Satterthwaite F-tests via anova(model, type = 2)
library(RColorBrewer)

# Use Satterthwaite df for emmeans/emtrends too, to match the F-tests
emmeans::emm_options(lmer.df = "satterthwaite")

# Data --------------------------------------------------------------------

myc_alldata <- read_csv(
  "outplut/alldata_03262026.csv",
  show_col_types = FALSE
) %>%
  mutate(
    site_unit = factor(site_unit),
    soil_cn_ratio = soil_percent_c / soil_percent_n
  )

myc_2023 <- myc_alldata %>%
  filter(year == 2023) %>%
  distinct() %>%
  droplevels()

# Mean-center distance to edge. Models use distance_c so that main effects of
# tree type and legacy are tested at the mean distance rather than at the edge
# (distance = 0, outside the data). Slopes and interaction tests are unchanged.
# Figures still plot raw distance: add distance_mean back to prediction grids.
distance_mean <- mean(myc_2023$distance_to_edge_m, na.rm = TRUE)

myc_2023 <- myc_2023 %>%
  mutate(distance_c = distance_to_edge_m - distance_mean)

# Shared appearance -------------------------------------------------------

blues <- RColorBrewer::brewer.pal(9, "Blues")
oranges <- RColorBrewer::brewer.pal(9, "Oranges")

myc_type_colors <- c("am" = blues[5], "ecm" = oranges[5])
legacy_shapes <- c("am" = 17, "ecm" = 16)
legacy_labels <- c("am" = "AM legacy", "ecm" = "EcM legacy")
myc_type_labels <- c("am" = "AM tree", "ecm" = "EcM tree")

my_theme <- function() {
  theme_classic() +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.position = "bottom",
      plot.tag = element_text(size = 11, face = "bold")
    )
}

add_point_scales <- function(plot) {
  plot +
    scale_color_manual(
      values = myc_type_colors,
      breaks = names(myc_type_labels),
      labels = myc_type_labels,
      name = "Tree mycorrhizal type"
    ) +
    scale_shape_manual(
      values = legacy_shapes,
      breaks = names(legacy_labels),
      labels = legacy_labels,
      name = "Mycorrhizal legacy"
    )
}

base_distance_plot <- function(raw_data, response, y_label) {
  plot <- ggplot() +
    geom_point(
      data = filter(
        raw_data,
        !is.na(distance_to_edge_m),
        !is.na(.data[[response]])
      ),
      aes(
        x = distance_to_edge_m,
        y = .data[[response]],
        color = myc_type,
        shape = mycorrhizal_legacy
      ),
      size = 2,
      alpha = 0.8
    ) +
    labs(
      x = "Distance from forest edge (m)",
      y = y_label
    ) +
    my_theme()

  add_point_scales(plot)
}

add_tree_prediction_layers <- function(plot, predicted_data) {
  plot +
    geom_ribbon(
      data = filter(predicted_data, myc_type == "am"),
      aes(
        x = distance_to_edge_m,
        ymin = lower.CL,
        ymax = upper.CL,
        group = myc_type
      ),
      fill = "grey80",
      alpha = 0.35,
      inherit.aes = FALSE
    ) +
    geom_line(
      data = predicted_data,
      aes(
        x = distance_to_edge_m,
        y = emmean,
        color = myc_type,
        linetype = myc_type,
        group = myc_type
      ),
      linewidth = 1.2,
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    scale_linetype_manual(
      values = c("am" = "solid", "ecm" = "dashed"),
      guide = "none"
    )
}

# Figure 2a: leaf N versus distance ---------------------------------------

mod_leafn_distedge <- lmer(
  leaf_percent_n ~
    distance_c * myc_type * mycorrhizal_legacy +
    condition + (1 | site_unit) + (1 | species),
  data = myc_2023
)

anova(mod_leafn_distedge, type = 2, ddf = "Satterthwaite")
summary(mod_leafn_distedge)
range(myc_2023$leaf_percent_n)


distance_grid <- seq(
  min(myc_2023$distance_to_edge_m, na.rm = TRUE),
  max(myc_2023$distance_to_edge_m, na.rm = TRUE),
  length.out = 100
)

pred_leafn_distedge <- emmeans::emmeans(
  mod_leafn_distedge,
  ~ distance_c,
  at = list(distance_c = distance_grid - distance_mean)
) %>%
  confint() %>%
  as_tibble() %>%
  mutate(distance_to_edge_m = distance_c + distance_mean)

fig_2a <- base_distance_plot(
  myc_2023,
  "leaf_percent_n",
  "Predicted leaf percent N"
) +
  geom_ribbon(
    data = pred_leafn_distedge,
    aes(x = distance_to_edge_m, ymin = lower.CL, ymax = upper.CL),
    fill = "grey80",
    alpha = 0.35,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_leafn_distedge,
    aes(x = distance_to_edge_m, y = emmean),
    color = "black",
    linewidth = 1.2,
    inherit.aes = FALSE
  )

# Figure 2b: seedling height versus distance ------------------------------


mod_height_distedge <- lmer(
  height_change ~
    leaf_percent_n +
    distance_c * myc_type * mycorrhizal_legacy +
    condition + (1 | site_unit) + (1 | species),
  data = myc_2023
)

summary (mod_height_distedge)
anova(mod_height_distedge, type = 2, ddf = "Satterthwaite")

pred_height_distedge <- emmeans::emmeans(
  mod_height_distedge,
  ~ myc_type | distance_c,
  at = list(
    distance_c = distance_grid - distance_mean,
    leaf_percent_n = mean(myc_2023$leaf_percent_n, na.rm = TRUE),
    foliar_15n_enrichment = mean(
      myc_2023$foliar_15n_enrichment,
      na.rm = TRUE
    )
  )
) %>%
  confint() %>%
  as_tibble() %>%
  mutate(distance_to_edge_m = distance_c + distance_mean)

fig_2b <- base_distance_plot(
  myc_2023,
  "height_change",
  "Predicted seedling height (cm)"
) %>%
  add_tree_prediction_layers(pred_height_distedge)

# Figure 2c: foliar delta-15N enrichment versus distance ------------------
mod_foliar15n_distedge <- lmer(
  foliar_15n_enrichment ~
    leaf_percent_n +
    distance_c * myc_type * mycorrhizal_legacy +
    condition + (1 | site_unit) + (1 | species),
  data = myc_2023
)

summary (mod_foliar15n_distedge)

anova(mod_foliar15n_distedge, type = 2, ddf = "Satterthwaite")

pred_foliar15n_distedge <- emmeans::emmeans(
  mod_foliar15n_distedge,
  ~ myc_type | distance_c,
  at = list(
    distance_c = distance_grid - distance_mean,
    leaf_percent_n = mean(myc_2023$leaf_percent_n, na.rm = TRUE)
  )
) %>%
  confint() %>%
  as_tibble() %>%
  mutate(distance_to_edge_m = distance_c + distance_mean)

fig_2c <- base_distance_plot(
  myc_2023,
  "foliar_15n_enrichment",
  expression(paste("Predicted foliar ", delta^{15}, "N enrichment"))
) %>%
  add_tree_prediction_layers(pred_foliar15n_distedge)

## Assemble and save Figure 2 ---------------------------------------------

figure_2_panel_a <- fig_2a + labs(x = NULL)
figure_2_panel_b <- fig_2b + labs(x = NULL)

figure_2 <- patchwork::wrap_plots(
  figure_2_panel_a,
  figure_2_panel_b,
  fig_2c,
  ncol = 1,
  guides = "collect"
) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "center",
    plot.tag = element_text(size = 14, face = "bold")
  )

dir.create("outplut/figures", showWarnings = FALSE, recursive = TRUE)

figure_2_parts <- list(
  "figure_2a_leaf_n_vs_distance_to_edge.png" = fig_2a,
  "figure_2b_seedling_height_vs_distance_to_edge.png" = fig_2b,
  "figure_2c_foliar_15n_vs_distance_to_edge.png" = fig_2c
)

iwalk(
  figure_2_parts,
  ~ ggsave(
    filename = file.path("outplut/figures", .y),
    plot = .x,
    width = 7,
    height = 5.5,
    dpi = 300,
    bg = "white"
  )
)

ggsave(
  "outplut/figures/figure_2.png",
  figure_2,
  width = 7,
  height = 14,
  dpi = 300,
  bg = "white"
)

ggsave(
  "outplut/figures/figure_2_editable.pdf",
  figure_2,
  width = 7,
  height = 14,
  device = "pdf",
  bg = "white"
)

ggsave(
  "outplut/figures/figure_2_editable.svg",
  figure_2,
  width = 7,
  height = 14,
  device = svglite::svglite,
  bg = "white"
)

# Figure S1: soil responses ----------------------------------------------

soil_data <- myc_2023 %>%
  filter(
    !is.na(distance_to_edge_m),
    !is.na(myc_type),
    !is.na(mycorrhizal_legacy)
  )

soil_scatter_plot <- function(data, response, y_label, y_limits) {
  ggplot(
    filter(data, !is.na(.data[[response]])),
    aes(
      x = distance_to_edge_m,
      y = .data[[response]],
      shape = mycorrhizal_legacy
    )
  ) +
    geom_point(size = 2, alpha = 0.8, color = "black") +
    scale_y_continuous(limits = y_limits) +
    scale_shape_manual(
      values = legacy_shapes,
      breaks = names(legacy_labels),
      labels = legacy_labels,
      name = "Mycorrhizal legacy"
    ) +
    labs(
      x = "Distance from forest edge (m)",
      y = y_label
    ) +
    my_theme()
}

soil_mean_plot <- function(predicted_data, y_label, y_limits) {
  ggplot(
    predicted_data,
    aes(
      x = mycorrhizal_legacy,
      y = emmean,
      shape = mycorrhizal_legacy
    )
  ) +
    geom_errorbar(
      aes(ymin = lower.CL, ymax = upper.CL),
      width = 0.12,
      linewidth = 0.6,
      color = "black"
    ) +
    geom_point(size = 1.7, color = "black") +
    scale_y_continuous(limits = y_limits) +
    scale_x_discrete(labels = legacy_labels) +
    scale_shape_manual(values = legacy_shapes, guide = "none") +
    labs(x = NULL, y = y_label) +
    my_theme() +
    theme(legend.position = "none")
}

mod_soil_n <- lmer(
  soil_percent_n ~
    distance_c * mycorrhizal_legacy +
    (1 | site_unit),
  data = soil_data
)
summary (mod_soil_n)
anova(mod_soil_n, type = 2, ddf = "Satterthwaite")

mod_soil_cn <- lmer(
  soil_cn_ratio ~
    distance_c * mycorrhizal_legacy +
    (1 | site_unit),
  data = soil_data
)
summary (mod_soil_cn)
anova(mod_soil_cn, type = 2, ddf = "Satterthwaite")

emmeans::emtrends (
  mod_soil_cn,
  ~ mycorrhizal_legacy,
  var = "distance_c"
)

mean_soil_distance_c <- mean(soil_data$distance_c, na.rm = TRUE)

soil_n_predictions <- emmeans::emmeans(
  mod_soil_n,
  ~ mycorrhizal_legacy,
  at = list(distance_c = mean_soil_distance_c)
) %>%
  confint() %>%
  as_tibble()

soil_cn_predictions <- emmeans::emmeans(
  mod_soil_cn,
  ~ mycorrhizal_legacy,
  at = list(distance_c = mean_soil_distance_c)
) %>%
  confint() %>%
  as_tibble()
soil_cn_predictions

# Pairwise legacy contrasts evaluated at the mean observed distance, matching
# the estimated marginal means displayed in the right-hand panels.
soil_n_legacy_contrast <- emmeans::emmeans(
  mod_soil_n,
  ~ mycorrhizal_legacy,
  at = list(distance_c = mean_soil_distance_c)
) %>%
  pairs() %>%
  summary(infer = c(TRUE, TRUE)) %>%
  as_tibble()

soil_cn_legacy_contrast <- emmeans::emmeans(
  mod_soil_cn,
  ~ mycorrhizal_legacy,
  at = list(distance_c = mean_soil_distance_c)
) %>%
  pairs() %>%
  summary(infer = c(TRUE, TRUE)) %>%
  as_tibble()

soil_n_limits <- range(
  soil_data$soil_percent_n,
  soil_n_predictions$lower.CL,
  soil_n_predictions$upper.CL,
  na.rm = TRUE
)

# Fitted C:N lines across distance for each legacy (Figure S1c)
soil_cn_lines <- emmeans::emmeans(
  mod_soil_cn,
  ~ mycorrhizal_legacy | distance_c,
  at = list(
    distance_c = seq(
      min(soil_data$distance_c),
      max(soil_data$distance_c),
      length.out = 100
    )
  )
) %>%
  confint() %>%
  as_tibble() %>%
  mutate(distance_to_edge_m = distance_c + distance_mean)

soil_cn_limits <- range(
  soil_data$soil_cn_ratio,
  soil_cn_predictions$lower.CL,
  soil_cn_predictions$upper.CL,
  soil_cn_lines$lower.CL,
  soil_cn_lines$upper.CL,
  na.rm = TRUE
)

fig_s1a <- soil_scatter_plot(
  soil_data,
  "soil_percent_n",
  "Soil N (%)",
  soil_n_limits
) +
  # legacy legend comes from panel C, which also shows the line types
  guides(shape = "none")

fig_s1b <- soil_mean_plot(
  soil_n_predictions,
  "Mean soil N (%)",
  soil_n_limits
)

fig_s1c <- soil_scatter_plot(
  soil_data,
  "soil_cn_ratio",
  "Soil C:N ratio",
  soil_cn_limits
) +
  geom_ribbon(
    data = soil_cn_lines,
    aes(
      x = distance_to_edge_m,
      ymin = lower.CL,
      ymax = upper.CL,
      group = mycorrhizal_legacy
    ),
    fill = "grey80",
    alpha = 0.35,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = soil_cn_lines,
    aes(
      x = distance_to_edge_m,
      y = emmean,
      linetype = mycorrhizal_legacy
    ),
    color = "black",
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  scale_linetype_manual(
    values = c("am" = "solid", "ecm" = "dashed"),
    breaks = names(legacy_labels),
    labels = legacy_labels,
    name = "Mycorrhizal legacy"
  )

# draw the raw points above the ribbons and lines
fig_s1c$layers <- c(fig_s1c$layers[-1], fig_s1c$layers[1])

fig_s1d <- soil_mean_plot(
  soil_cn_predictions,
  "Mean soil C:N ratio",
  soil_cn_limits
)

figure_s1 <- patchwork::wrap_plots(
  fig_s1a,
  fig_s1b,
  fig_s1c,
  fig_s1d,
  design = "AB\nCD",
  widths = c(3, 1),
  guides = "collect"
) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

ggsave(
  "outplut/figures/figure_s1_soil_n_and_cn_ratio.png",
  figure_s1,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# Supplemental table for all model results -------------------------------

dir.create("outplut/tables", showWarnings = FALSE, recursive = TRUE)

table_models <- list(
  "Leaf N (%)" = mod_leafn_distedge,
  "Height change (cm)" = mod_height_distedge,
  "Foliar δ15N enrichment (‰)" = mod_foliar15n_distedge,
  "Soil N (%)" = mod_soil_n,
  "Soil C:N ratio" = mod_soil_cn
)

variable_labels <- c(
  "distance_c" = "Distance to edge",
  "myc_type" = "Tree mycorrhizal type",
  "mycorrhizal_legacy" = "Mycorrhizal legacy",
  "leaf_percent_n" = "Leaf N",
  "condition" = "Condition"
)

format_p <- function(p) {
  ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))
}

# Type II F-tests with Satterthwaite denominator df (lmerTest), one row per term
satterthwaite_f <- function(model) {
  anova(model, type = 2, ddf = "Satterthwaite") %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    as_tibble()
}

# Make the table
table_models %>%

  # Type II F-tests (Satterthwaite df) for each model
  purrr::imap_dfr(~ {
    satterthwaite_f(.x) %>%
      mutate(
        Model = .y,
        Variable = term %>%
          str_split(":") %>%
          map_chr(~ paste(variable_labels[.x], collapse = " * ")),
        `p value` = format_p(`Pr(>F)`),
        F = round(`F value`, 2),
        `Num df` = NumDF,
        `Den df` = round(DenDF, 1)
      ) %>%
      select(Model, Variable, F, `Num df`, `Den df`, `p value`)
  }) %>%

  # Format for table display
  group_by(Model) %>%
  mutate(Model = if_else(row_number() == 1, Model, "")) %>%
  ungroup() %>%

  # Make the flextable
  flextable::flextable() %>%
  flextable::set_header_labels(
    Model = "Model",
    Variable = "Variable",
    `p value` = "p value"
  ) %>%
  flextable::theme_vanilla() %>%
  flextable::bold(part = "header") %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::autofit() %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 12, part = "all") %>%
  flextable::save_as_docx(path = "outplut/tables/model_results_10122026.docx")


# table for emtrends results

# Distance slopes for each tree type x legacy group (soil models: by legacy).
# p value tests whether each slope differs from zero.
table_models %>%

  purrr::imap_dfr(~ {
    trend_specs <- if ("myc_type" %in% all.vars(formula(.x))) {
      ~ myc_type * mycorrhizal_legacy
    } else {
      ~ mycorrhizal_legacy
    }

    emmeans::emtrends(.x, specs = trend_specs, var = "distance_c") %>%
      summary(infer = c(TRUE, TRUE)) %>%
      as.data.frame() %>%
      mutate(
        Model = .y,
        `Tree mycorrhizal type` = if ("myc_type" %in% names(.)) {
          myc_type_labels[as.character(myc_type)]
        } else {
          ""
        },
        `Mycorrhizal legacy` = legacy_labels[as.character(mycorrhizal_legacy)],
        Estimate = round(distance_c.trend, 3),
        `Standard error` = round(SE, 3),
        df = round(df, 1),
        `p value` = format_p(p.value)
      ) %>%
      select(
        Model, `Tree mycorrhizal type`, `Mycorrhizal legacy`,
        Estimate, `Standard error`, df, `p value`
      )
  }) %>%

  dplyr::group_by(Model) %>%
  dplyr::mutate(Model = ifelse(dplyr::row_number() == 1, Model, "")) %>%
  dplyr::ungroup() %>%

  # make the table

  flextable::flextable() %>%
  flextable::theme_vanilla() %>%
  flextable::bold(part = "header") %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::autofit() %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 12, part = "all") %>%

  # Export to Word file
  flextable::save_as_docx(path = "outplut/tables/emtrends_results_10122026.docx")


# Supplemental tables S1-S3 as one Excel workbook --------------------------
# One sheet per table. Values are rounded but numeric so they stay sortable;
# p values below 0.001 are written as "<0.001".

excel_p <- function(p) {
  ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 4))
}

term_labels <- c(
  "distance_c" = "Distance to edge",
  "myc_type" = "Tree mycorrhizal type",
  "mycorrhizal_legacy" = "Mycorrhizal legacy",
  "leaf_percent_n" = "Leaf N",
  "condition" = "Condition"
)

f_table <- function(models) {
  purrr::imap_dfr(models, ~ {
    satterthwaite_f(.x) %>%
      mutate(
        Response = .y,
        Term = term %>%
          str_split(":") %>%
          map_chr(~ paste(term_labels[.x], collapse = " × ")),
        `Num df` = NumDF,
        `Den df` = round(DenDF, 1),
        F = round(`F value`, 4),
        p = excel_p(`Pr(>F)`)
      ) %>%
      select(Response, Term, `Num df`, `Den df`, F, p)
  })
}

# Table S1: Satterthwaite F-tests for the Figure 2 models
table_s1 <- f_table(list(
  "Leaf N (%)" = mod_leafn_distedge,
  "Δ height (cm)" = mod_height_distedge,
  "Foliar δ15N enrichment (‰)" = mod_foliar15n_distedge
))

# Table S2: marginal distance slopes used for Figure 2. Leaf N has no
# distance x tree type interaction, so one pooled slope; height and foliar
# enrichment get a slope per tree mycorrhizal type (averaged over legacy).
trend_rows <- function(model, specs, panel, response) {
  emmeans::emtrends(model, specs = specs, var = "distance_c") %>%
    summary(infer = c(TRUE, TRUE)) %>%
    as.data.frame() %>%
    mutate(
      Panel = panel,
      Response = response,
      `Tree mycorrhizal type` = if ("myc_type" %in% names(.)) {
        myc_type_labels[as.character(myc_type)]
      } else {
        "All"
      },
      Estimate = round(distance_c.trend, 4),
      SE = round(SE, 4),
      df = round(df, 0),
      t = round(t.ratio, 4),
      p = excel_p(p.value),
      `95% CI` = sprintf("[%.3f, %.3f]", lower.CL, upper.CL)
    ) %>%
    select(
      Panel, Response, `Tree mycorrhizal type`,
      Estimate, SE, df, t, p, `95% CI`
    )
}

table_s2 <- bind_rows(
  trend_rows(mod_leafn_distedge, ~ 1, "2a", "Leaf N (%)"),
  trend_rows(mod_height_distedge, ~ myc_type, "2b", "Δ height (cm)"),
  trend_rows(
    mod_foliar15n_distedge, ~ myc_type, "2c", "Foliar δ15N enrichment (‰)"
  )
)

# Table S3: Satterthwaite F-tests for the soil chemistry models (Figure S1)
table_s3 <- f_table(list(
  "Soil N (%)" = mod_soil_n,
  "Soil C:N ratio" = mod_soil_cn
))

supplemental_tables <- list(
  "Table S1" = list(
    title = paste(
      "Table S1: Type II F-tests (Satterthwaite denominator df) on mixed",
      "effects models. Distance to edge was mean-centered."
    ),
    data = table_s1
  ),
  "Table S2" = list(
    title = paste(
      "Table S2: Marginal distance trends used to determine significance of",
      "separate responses by mycorrhizal type for Figure 2."
    ),
    data = table_s2
  ),
  "Table S3" = list(
    title = paste(
      "Table S3: Type II F-tests (Satterthwaite denominator df) on soil",
      "chemistry mixed effects models. Distance to edge was mean-centered."
    ),
    data = table_s3
  )
)

wb <- openxlsx::createWorkbook()
header_style <- openxlsx::createStyle(
  textDecoration = "bold", border = "bottom", halign = "center"
)
title_style <- openxlsx::createStyle(textDecoration = "bold")

iwalk(supplemental_tables, ~ {
  openxlsx::addWorksheet(wb, .y)
  openxlsx::writeData(wb, .y, .x$title, startRow = 1)
  openxlsx::addStyle(wb, .y, title_style, rows = 1, cols = 1)
  openxlsx::writeData(
    wb, .y, .x$data, startRow = 3, headerStyle = header_style
  )
  openxlsx::setColWidths(wb, .y, cols = seq_along(.x$data), widths = "auto")
})

openxlsx::saveWorkbook(
  wb,
  "outplut/tables/supplemental_tables_S1-S3_11092026.xlsx",
  overwrite = TRUE
)
