

# Outtake figure analyses -------------------------------------------------
# Archived from 05_final_figures.R when the manuscript figure set was
# narrowed to Figure 2 and the soil supplemental figure.

# setup -------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(RColorBrewer)


# read in data

myc_alldata <-   
  read_csv("outplut/alldata_03262026.csv") %>% 
  mutate(
    site_unit = factor(site_unit),
    soil_cn_ratio = soil_percent_c / soil_percent_n
  ) 

myc_2023 <- 
  myc_alldata %>%
  filter(year == "2023") %>%
  droplevels() %>%
  
  # turn the factors into numeric variables
  
  mutate(
    myc_legacy_num = case_when(mycorrhizal_legacy == "ecm" ~ 0, TRUE ~ 1),
    myc_type_num = case_when(myc_type == "ecm" ~ 0, TRUE ~ 1),
    
    # scale numeric variables
    
    dist_s = scale(distance_to_edge_m)[,1]
  )

# Plotting theme ---------------------------------------------------------

blues <- RColorBrewer::brewer.pal(9, "Blues")
oranges <- RColorBrewer::brewer.pal(9, "Oranges")

my_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = element_text(size = 8),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      strip.text.x = element_text(size = 8),
      legend.position = "bottom"
    )
}

myc_association_colors <- c("am" = blues[5], "ecm" = oranges[5])
legacy_shapes <- c("am" = 17, "ecm" = 16)

# Build predictions only across the observed x range of each plotted group.
# This prevents a line or confidence ribbon from extrapolating beyond that
# group's points within a legacy-plot panel.
group_prediction_grid <- function(data, x_var, y_var, length_out = 100) {
  data %>%
    filter(
      !is.na(.data[[x_var]]),
      !is.na(.data[[y_var]]),
      !is.na(myc_type),
      !is.na(mycorrhizal_legacy)
    ) %>%
    group_by(myc_type, mycorrhizal_legacy) %>%
    summarise(
      prediction_x = list(seq(
        min(.data[[x_var]], na.rm = TRUE),
        max(.data[[x_var]], na.rm = TRUE),
        length.out = length_out
      )),
      .groups = "drop"
    ) %>%
    unnest(prediction_x) %>%
    rename(!!x_var := prediction_x)
}

add_fixed_effect_ci <- function(model, newdata, fixed_formula, pred_name) {
  newdata[[pred_name]] <- predict(model, newdata = newdata, re.form = NA)
  
  x_pred <- model.matrix(fixed_formula, data = newdata)
  se_pred <- sqrt(diag(x_pred %*% vcov(model) %*% t(x_pred)))
  
  newdata %>%
    mutate(
      se = se_pred,
      lower = .data[[pred_name]] - 1.96 * se,
      upper = .data[[pred_name]] + 1.96 * se
    )
}

add_slope_linetype <- function(model, predicted_data, x_var, alpha = 0.05) {
  slope_tests <-
    emmeans::emtrends(
      model,
      ~ myc_type * mycorrhizal_legacy,
      var = x_var
    ) %>%
    summary(infer = c(TRUE, TRUE)) %>%
    as_tibble() %>%
    mutate(
      slope_linetype = case_when(
        p.value < alpha ~ "Significant",
        TRUE ~ "Not significant"
      )
    ) %>%
    select(myc_type, mycorrhizal_legacy, slope_linetype)
  
  predicted_data %>%
    left_join(slope_tests, by = c("myc_type", "mycorrhizal_legacy"))
}

get_column_or_na <- function(df, column_names) {
  matching_column <- intersect(column_names, names(df))
  
  if (length(matching_column) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  df[[matching_column[1]]]
}

tidy_fixed_effects <- function(model, figure, response, predictor) {
  fixed_effects <-
    coef(summary(lmerTest::as_lmerModLmerTest(model))) %>%
    as.data.frame()
  
  fixed_effects$term <- rownames(fixed_effects)
  
  tibble(
    figure = figure,
    response = response,
    predictor = predictor,
    statistic_type = "fixed_effect",
    term = fixed_effects$term,
    myc_type = NA_character_,
    mycorrhizal_legacy = NA_character_,
    estimate = get_column_or_na(fixed_effects, "Estimate"),
    std_error = get_column_or_na(fixed_effects, "Std. Error"),
    df = get_column_or_na(fixed_effects, "df"),
    statistic = get_column_or_na(fixed_effects, "t value"),
    p_value = get_column_or_na(fixed_effects, "Pr(>|t|)"),
    conf_low = NA_real_,
    conf_high = NA_real_,
    slope_linetype = NA_character_
  )
}

tidy_type_ii_tests <- function(model, figure, response, predictor) {
  type_ii_tests <- car::Anova(model) %>% as.data.frame()
  type_ii_tests$term <- rownames(type_ii_tests)
  
  tibble(
    figure = figure,
    response = response,
    predictor = predictor,
    statistic_type = "type_ii_wald_chisq",
    term = type_ii_tests$term,
    myc_type = NA_character_,
    mycorrhizal_legacy = NA_character_,
    estimate = NA_real_,
    std_error = NA_real_,
    df = get_column_or_na(type_ii_tests, "Df"),
    statistic = get_column_or_na(type_ii_tests, "Chisq"),
    p_value = get_column_or_na(type_ii_tests, "Pr(>Chisq)"),
    conf_low = NA_real_,
    conf_high = NA_real_,
    slope_linetype = NA_character_
  )
}

tidy_slope_tests <- function(model, figure, response, predictor, x_var, alpha = 0.05) {
  slope_tests <-
    emmeans::emtrends(
      model,
      ~ myc_type * mycorrhizal_legacy,
      var = x_var
    ) %>%
    summary(infer = c(TRUE, TRUE)) %>%
    as_tibble()
  
  trend_column <- paste0(x_var, ".trend")
  
  tibble(
    figure = figure,
    response = response,
    predictor = predictor,
    statistic_type = "simple_slope",
    term = paste0("slope of ", response, " over ", predictor),
    myc_type = slope_tests$myc_type,
    mycorrhizal_legacy = slope_tests$mycorrhizal_legacy,
    estimate = slope_tests[[trend_column]],
    std_error = slope_tests$SE,
    df = slope_tests$df,
    statistic = slope_tests$t.ratio,
    p_value = slope_tests$p.value,
    conf_low = slope_tests$lower.CL,
    conf_high = slope_tests$upper.CL,
    slope_linetype = case_when(
      slope_tests$p.value < alpha ~ "Significant",
      TRUE ~ "Not significant"
    )
  )
}

collect_model_stats <- function(model, figure, response, predictor, x_var) {
  bind_rows(
    tidy_fixed_effects(model, figure, response, predictor),
    tidy_type_ii_tests(model, figure, response, predictor),
    tidy_slope_tests(model, figure, response, predictor, x_var)
  )
}

panel_label_data <- function(plot_data, predicted_data) {
  tibble(
    mycorrhizal_legacy = c("am", "ecm"),
    panel_label = c("A", "B"),
    x = min(plot_data$x, na.rm = TRUE),
    y = max(predicted_data$upper, na.rm = TRUE)
  )
}

model_line_plot <- function(
    raw_data,
    predicted_data,
    x_var,
    y_var,
    pred_var,
    x_label,
    y_label
) {
  label_data <- panel_label_data(
    tibble(x = raw_data[[x_var]]),
    predicted_data
  )
  
  ggplot() +
    geom_point(
      data = raw_data,
      aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        color = myc_type,
        fill = myc_type
      )
    ) +
    geom_line(
      data = predicted_data,
      aes(
        x = .data[[x_var]],
        y = .data[[pred_var]],
        color = factor(myc_type),
        linetype = slope_linetype
      ),
      linewidth = 2
    ) +
    geom_ribbon(
      data = filter(predicted_data, slope_linetype == "Significant"),
      aes(
        x = .data[[x_var]],
        ymin = lower,
        ymax = upper,
        fill = factor(myc_type),
        group = interaction(myc_type, mycorrhizal_legacy)
      ),
      alpha = 0.2
    ) +
    facet_wrap(
      ~mycorrhizal_legacy,
      labeller = as_labeller(c(
        "am" = "AM legacy plot",
        "ecm" = "EcM legacy plot"
      ))
    ) +
    labs(
      y = y_label,
      x = x_label,
      color = "Mycorrhizal association",
      fill = "Mycorrhizal association"
    ) +
    scale_color_manual(
      values = myc_association_colors,
      limits = names(myc_association_colors),
      name = "Mycorrhizal association"
    ) +
    scale_fill_manual(
      values = myc_association_colors,
      limits = names(myc_association_colors),
      guide = "none"
    ) +
    scale_linetype_manual(
      values = c("Significant" = "solid", "Not significant" = "dashed"),
      guide = "none"
    ) +
    geom_text(
      data = label_data,
      aes(x = x, y = y, label = panel_label),
      hjust = 0,
      size = 4,
      inherit.aes = FALSE
    ) +
    my_theme()
}

# Figure 2b: Height change ~ distance to edge -----------------------------

# height change

mod_2_alt <-
  lme4::lmer(height_change ~  
               leaf_percent_n  * myc_type + 
               foliar_15n_enrichment * myc_type + 
               distance_to_edge_m * myc_type * mycorrhizal_legacy +
               #herbivory +
               (1 | condition ) + 
               (1 | site_unit)  + (1 | species) ,
             data = myc_2023)

AIC(mod_2_alt)
summary(mod_2_alt)
car::Anova(mod_2_alt)
plot(resid(mod_2_alt)~ myc_2023$leaf_percent_n)

emmeans::emtrends(mod_2_alt ,~ myc_type * mycorrhizal_legacy, var = "leaf_percent_n")
emmeans::emtrends(mod_2_alt , ~ myc_type, var = "leaf_percent_n")

emmeans::emtrends(mod_2_alt , ~ distance_to_edge_m*myc_type, var = "distance_to_edge_m")

# Average predictions across AM and EcM tree associations to obtain one
# fitted line for each legacy type, matching Figure 2.
height_legacy_grid <- myc_2023 %>%
  filter(
    !is.na(distance_to_edge_m),
    !is.na(height_change),
    !is.na(myc_type),
    !is.na(mycorrhizal_legacy)
  ) %>%
  group_by(mycorrhizal_legacy) %>%
  reframe(
    distance_to_edge_m = seq(
      min(distance_to_edge_m),
      max(distance_to_edge_m),
      length.out = 100
    )
  ) %>%
  tidyr::expand_grid(
    myc_type = sort(unique(na.omit(myc_2023$myc_type)))
  ) %>%
  mutate(
    leaf_percent_n = mean(myc_2023$leaf_percent_n, na.rm = TRUE),
    foliar_15n_enrichment = mean(
      myc_2023$foliar_15n_enrichment,
      na.rm = TRUE
    )
  )

pred_height_distedge <- height_legacy_grid %>%
  mutate(
    pred_height = predict(mod_2_alt, newdata = ., re.form = NA)
  ) %>%
  group_by(mycorrhizal_legacy, distance_to_edge_m) %>%
  summarise(
    pred_height = mean(pred_height),
    .groups = "drop"
  )

fig_3_height_distedge <- ggplot() +
  geom_point(
    data = filter(
      myc_2023,
      !is.na(distance_to_edge_m),
      !is.na(height_change)
    ),
    aes(
      x = distance_to_edge_m,
      y = height_change,
      color = myc_type,
      shape = mycorrhizal_legacy
    ),
    size = 2,
    alpha = 0.8
  ) +
  geom_line(
    data = filter(pred_height_distedge, mycorrhizal_legacy == "am"),
    aes(x = distance_to_edge_m, y = pred_height),
    color = "black",
    linetype = "solid",
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  geom_line(
    data = filter(pred_height_distedge, mycorrhizal_legacy == "ecm"),
    aes(x = distance_to_edge_m, y = pred_height),
    color = "grey55",
    linetype = "dashed",
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = myc_association_colors,
    breaks = c("am", "ecm"),
    labels = c("AM tree", "EcM tree"),
    name = "Tree mycorrhizal type"
  ) +
  scale_shape_manual(
    values = legacy_shapes,
    breaks = c("am", "ecm"),
    labels = c("AM legacy", "EcM legacy"),
    name = "Mycorrhizal legacy"
  ) +
  labs(
    y = "Predicted seedling height (cm)",
    x = "Distance from forest edge (m)"
  ) +
  my_theme()

print(fig_3_height_distedge)


# Figure 2c: Foliar 15N enrichment ~ distance to edge ---------------------

mod_foliar15n_distedge <-
  lme4::lmer(
    foliar_15n_enrichment ~
      leaf_percent_n +
      distance_to_edge_m * myc_type * mycorrhizal_legacy +
      (1 | site_unit) + (1 | species),
    data = myc_2023
  )

# Average predictions across tree association to obtain one fitted line per
# legacy type, using the same construction and styling as Figures 2a and 2b.
foliar15n_legacy_grid <- myc_2023 %>%
  filter(
    !is.na(distance_to_edge_m),
    !is.na(foliar_15n_enrichment),
    !is.na(myc_type),
    !is.na(mycorrhizal_legacy)
  ) %>%
  group_by(mycorrhizal_legacy) %>%
  reframe(
    distance_to_edge_m = seq(
      min(distance_to_edge_m),
      max(distance_to_edge_m),
      length.out = 100
    )
  ) %>%
  tidyr::expand_grid(
    myc_type = sort(unique(na.omit(myc_2023$myc_type)))
  ) %>%
  mutate(
    leaf_percent_n = mean(myc_2023$leaf_percent_n, na.rm = TRUE)
  )

pred_foliar15n_distedge <- foliar15n_legacy_grid %>%
  mutate(
    pred_foliar_15n = predict(
      mod_foliar15n_distedge,
      newdata = .,
      re.form = NA
    )
  ) %>%
  group_by(mycorrhizal_legacy, distance_to_edge_m) %>%
  summarise(
    pred_foliar_15n = mean(pred_foliar_15n),
    .groups = "drop"
  )

fig_2c_foliar15n_distedge <- ggplot() +
  geom_point(
    data = filter(
      myc_2023,
      !is.na(distance_to_edge_m),
      !is.na(foliar_15n_enrichment)
    ),
    aes(
      x = distance_to_edge_m,
      y = foliar_15n_enrichment,
      color = myc_type,
      shape = mycorrhizal_legacy
    ),
    size = 2,
    alpha = 0.8
  ) +
  geom_line(
    data = filter(pred_foliar15n_distedge, mycorrhizal_legacy == "am"),
    aes(x = distance_to_edge_m, y = pred_foliar_15n),
    color = "black",
    linetype = "solid",
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  geom_line(
    data = filter(pred_foliar15n_distedge, mycorrhizal_legacy == "ecm"),
    aes(x = distance_to_edge_m, y = pred_foliar_15n),
    color = "grey55",
    linetype = "dashed",
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = myc_association_colors,
    breaks = c("am", "ecm"),
    labels = c("AM tree", "EcM tree"),
    name = "Tree mycorrhizal type"
  ) +
  scale_shape_manual(
    values = legacy_shapes,
    breaks = c("am", "ecm"),
    labels = c("AM legacy", "EcM legacy"),
    name = "Mycorrhizal legacy"
  ) +
  labs(
    x = "Distance from forest edge (m)",
    y = expression(paste("Predicted foliar ", delta^{15}, "N enrichment"))
  ) +
  my_theme()

print(fig_2c_foliar15n_distedge)


# Figure 3: Foliar 15N enrichment ~ foliar percent N ----------------------

mod_foliar15n_leafn <-
  lme4::lmer(
    foliar_15n_enrichment ~
      leaf_percent_n * myc_type * mycorrhizal_legacy +
      distance_to_edge_m +
      (1 | site_unit) + (1 | species),
    data = myc_2023
  )

pred_foliar15n_leafn <- group_prediction_grid(
  myc_2023, "leaf_percent_n", "foliar_15n_enrichment"
) %>%
  mutate(
    distance_to_edge_m = mean(myc_2023$distance_to_edge_m, na.rm = TRUE)
  ) %>%
  add_fixed_effect_ci(
    model = mod_foliar15n_leafn,
    fixed_formula = ~ leaf_percent_n * myc_type * mycorrhizal_legacy +
      distance_to_edge_m,
    pred_name = "pred_foliar_15n"
  ) %>%
  add_slope_linetype(
    model = mod_foliar15n_leafn,
    x_var = "leaf_percent_n"
  )

fig_3_foliar15n_leafn <- model_line_plot(
  raw_data = myc_2023,
  predicted_data = pred_foliar15n_leafn,
  x_var = "leaf_percent_n",
  y_var = "foliar_15n_enrichment",
  pred_var = "pred_foliar_15n",
  x_label = "Leaf percent N",
  y_label = "Predicted foliar 15N enrichment"
)

print(fig_3_foliar15n_leafn)


# Figure 2a: Foliar percent N ~ distance to edge --------------------------

mod_leafn_distedge <-
  lme4::lmer(
    leaf_percent_n ~
      distance_to_edge_m * myc_type * mycorrhizal_legacy +
      (1 | condition) + (1 | site_unit) + (1 | species),
    data = myc_2023
  )

# Use one prediction line per legacy type. Predictions are averaged equally
# across AM and EcM tree associations while retaining the full fitted model.
leafn_legacy_grid <- myc_2023 %>%
  filter(
    !is.na(distance_to_edge_m),
    !is.na(leaf_percent_n),
    !is.na(myc_type),
    !is.na(mycorrhizal_legacy)
  ) %>%
  group_by(mycorrhizal_legacy) %>%
  reframe(
    distance_to_edge_m = seq(
      min(distance_to_edge_m),
      max(distance_to_edge_m),
      length.out = 100
    )
  ) %>%
  tidyr::expand_grid(
    myc_type = sort(unique(na.omit(myc_2023$myc_type)))
  )

pred_leafn_distedge <- leafn_legacy_grid %>%
  mutate(
    pred_leaf_percent_n = predict(
      mod_leafn_distedge,
      newdata = .,
      re.form = NA
    )
  ) %>%
  group_by(mycorrhizal_legacy, distance_to_edge_m) %>%
  summarise(
    pred_leaf_percent_n = mean(pred_leaf_percent_n),
    .groups = "drop"
  )

fig_2_leafn_distedge <- ggplot() +
  geom_point(
    data = filter(
      myc_2023,
      !is.na(distance_to_edge_m),
      !is.na(leaf_percent_n)
    ),
    aes(
      x = distance_to_edge_m,
      y = leaf_percent_n,
      color = myc_type,
      shape = mycorrhizal_legacy
    ),
    size = 2,
    alpha = 0.8
  ) +
  geom_line(
    data = filter(pred_leafn_distedge, mycorrhizal_legacy == "am"),
    aes(x = distance_to_edge_m, y = pred_leaf_percent_n),
    color = "black",
    linetype = "solid",
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  geom_line(
    data = filter(pred_leafn_distedge, mycorrhizal_legacy == "ecm"),
    aes(x = distance_to_edge_m, y = pred_leaf_percent_n),
    color = "grey55",
    linetype = "dashed",
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = myc_association_colors,
    breaks = c("am", "ecm"),
    labels = c("AM tree", "EcM tree"),
    name = "Tree mycorrhizal type",
    aesthetics = "colour"
  ) +
  scale_shape_manual(
    values = legacy_shapes,
    breaks = c("am", "ecm"),
    labels = c("AM legacy", "EcM legacy"),
    name = "Mycorrhizal legacy"
  ) +
  labs(
    x = "Distance from forest edge (m)",
    y = "Predicted leaf percent N"
  ) +
  my_theme()

print(fig_2_leafn_distedge)


# Figure 5: Height change ~ foliar percent N ------------------------------

pred_height_leafn <- group_prediction_grid(
  myc_2023, "leaf_percent_n", "height_change"
) %>%
  mutate(
    foliar_15n_enrichment = mean(myc_2023$foliar_15n_enrichment, na.rm = TRUE),
    distance_to_edge_m = mean(myc_2023$distance_to_edge_m, na.rm = TRUE)
  ) %>%
  add_fixed_effect_ci(
    model = mod_2_alt,
    fixed_formula = ~ leaf_percent_n * myc_type +
      foliar_15n_enrichment * myc_type +
      distance_to_edge_m * myc_type * mycorrhizal_legacy,
    pred_name = "pred_height"
  ) %>%
  add_slope_linetype(
    model = mod_2_alt,
    x_var = "leaf_percent_n"
  )

fig_5_height_leafn <- model_line_plot(
  raw_data = myc_2023,
  predicted_data = pred_height_leafn,
  x_var = "leaf_percent_n",
  y_var = "height_change",
  pred_var = "pred_height",
  x_label = "Leaf percent N",
  y_label = "Predicted seedling height (cm)"
)

print(fig_5_height_leafn)


# Figure 6: Soil delta 15N ~ distance to edge -----------------------------

mod_soil15n_distedge <-
  lme4::lmer(
    soil_d15n ~
      distance_to_edge_m * myc_type * mycorrhizal_legacy +
      (1 | site_unit) + (1 | species),
    data = myc_2023
  )

pred_soil15n_distedge <- group_prediction_grid(
  myc_2023, "distance_to_edge_m", "soil_d15n"
) %>%
  add_fixed_effect_ci(
    model = mod_soil15n_distedge,
    fixed_formula = ~ distance_to_edge_m * myc_type * mycorrhizal_legacy,
    pred_name = "pred_soil_d15n"
  ) %>%
  add_slope_linetype(
    model = mod_soil15n_distedge,
    x_var = "distance_to_edge_m"
  )

fig_6_soil15n_distedge <- model_line_plot(
  raw_data = myc_2023,
  predicted_data = pred_soil15n_distedge,
  x_var = "distance_to_edge_m",
  y_var = "soil_d15n",
  pred_var = "pred_soil_d15n",
  x_label = "Distance from forest edge (m)",
  y_label = expression(paste("Predicted soil ", delta^{15}, "N (\u2030)"))
)

print(fig_6_soil15n_distedge)


# Figure 7: Soil C:N ratio ~ distance to edge -----------------------------

mod_soil_cn_distedge <-
  lme4::lmer(
    soil_cn_ratio ~
      distance_to_edge_m * myc_type * mycorrhizal_legacy +
      (1 | site_unit) + (1 | species),
    data = myc_2023
  )

pred_soil_cn_distedge <- group_prediction_grid(
  myc_2023, "distance_to_edge_m", "soil_cn_ratio"
) %>%
  add_fixed_effect_ci(
    model = mod_soil_cn_distedge,
    fixed_formula = ~ distance_to_edge_m * myc_type * mycorrhizal_legacy,
    pred_name = "pred_soil_cn_ratio"
  ) %>%
  add_slope_linetype(
    model = mod_soil_cn_distedge,
    x_var = "distance_to_edge_m"
  )

fig_7_soil_cn_distedge <- model_line_plot(
  raw_data = myc_2023,
  predicted_data = pred_soil_cn_distedge,
  x_var = "distance_to_edge_m",
  y_var = "soil_cn_ratio",
  pred_var = "pred_soil_cn_ratio",
  x_label = "Distance from forest edge (m)",
  y_label = "Predicted soil C:N ratio"
)

print(fig_7_soil_cn_distedge)


# Save manuscript figures -------------------------------------------------

dir.create("outplut/figures", showWarnings = FALSE, recursive = TRUE)

# Figure 2 combines the three distance-to-edge responses. The shared legends
# are collected beneath the panels, and the individual plots are also saved.
figure_2 <- patchwork::wrap_plots(
  fig_2_leafn_distedge,
  fig_3_height_distedge,
  fig_2c_foliar15n_distedge,
  ncol = 1,
  guides = "collect"
) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

manuscript_figures <- list(
  "figure_2a_leaf_n_vs_distance_to_edge.png" = fig_2_leafn_distedge,
  "figure_2b_seedling_height_vs_distance_to_edge.png" = fig_3_height_distedge,
  "figure_2c_foliar_15n_vs_distance_to_edge.png" = fig_2c_foliar15n_distedge,
  "final_foliar_15n_vs_leaf_percent_n.png" = fig_3_foliar15n_leafn,
  "final_height_change_vs_leaf_percent_n.png" = fig_5_height_leafn,
  "final_soil_d15n_vs_distance_to_edge.png" = fig_6_soil15n_distedge,
  "final_soil_cn_ratio_vs_distance_to_edge.png" = fig_7_soil_cn_distedge
)

iwalk(
  manuscript_figures,
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
  filename = "outplut/figures/figure_2.png",
  plot = figure_2,
  width = 7,
  height = 14,
  dpi = 300,
  bg = "white"
)


# Export model statistics -------------------------------------------------

final_figure_model_stats <- bind_rows(
  collect_model_stats(
    model = mod_2_alt,
    figure = "Figure 2b",
    response = "height_change",
    predictor = "distance_to_edge_m",
    x_var = "distance_to_edge_m"
  ),
  collect_model_stats(
    model = mod_foliar15n_distedge,
    figure = "Figure 2c",
    response = "foliar_15n_enrichment",
    predictor = "distance_to_edge_m",
    x_var = "distance_to_edge_m"
  ),
  collect_model_stats(
    model = mod_foliar15n_leafn,
    figure = "Foliar 15N enrichment ~ leaf percent N",
    response = "foliar_15n_enrichment",
    predictor = "leaf_percent_n",
    x_var = "leaf_percent_n"
  ),
  collect_model_stats(
    model = mod_leafn_distedge,
    figure = "Figure 2a",
    response = "leaf_percent_n",
    predictor = "distance_to_edge_m",
    x_var = "distance_to_edge_m"
  ),
  collect_model_stats(
    model = mod_2_alt,
    figure = "Figure 5",
    response = "height_change",
    predictor = "leaf_percent_n",
    x_var = "leaf_percent_n"
  ),
  collect_model_stats(
    model = mod_soil15n_distedge,
    figure = "Figure 6",
    response = "soil_d15n",
    predictor = "distance_to_edge_m",
    x_var = "distance_to_edge_m"
  ),
  collect_model_stats(
    model = mod_soil_cn_distedge,
    figure = "Figure 7",
    response = "soil_cn_ratio",
    predictor = "distance_to_edge_m",
    x_var = "distance_to_edge_m"
  )
)

dir.create("outplut", showWarnings = FALSE, recursive = TRUE)
write_csv(
  final_figure_model_stats,
  "outplut/final_figure_model_stats.csv"
)
