

# setup -------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(RColorBrewer)


# read in data

myc_alldata <-   
  read_csv("outplut/alldata_03262026.csv") %>% 
  mutate(
    site_unit = factor(site_unit)
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

# Height change ~ distance to edge  ------------------------------------------------------------

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

# create predicted dataset

# predicted values for plotting CIs and trend lines 

predicted_data <- expand_grid(
  distance_to_edge_m = seq(
    min(myc_2023$distance_to_edge_m, na.rm = TRUE),
    max(myc_2023$distance_to_edge_m, na.rm = TRUE),
    length.out = 100
  ),
  myc_type = unique(myc_2023$myc_type),
  mycorrhizal_legacy = unique(myc_2023$mycorrhizal_legacy)
) %>%
  mutate(
    leaf_percent_n = mean(myc_2023$leaf_percent_n, na.rm = TRUE),
    foliar_15n_enrichment = mean(
      myc_2023$foliar_15n_enrichment,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    pred_height = predict(mod_2_alt, newdata = ., re.form = NA)
  )

X_pred <- model.matrix(
  ~  leaf_percent_n * myc_type +
    foliar_15n_enrichment * myc_type +
    distance_to_edge_m * myc_type * mycorrhizal_legacy,
  data = predicted_data
)

se_preds <- sqrt(
  diag(X_pred %*% vcov(mod_2_alt) %*% t(X_pred))
)

predicted_data <- 
  predicted_data %>%
  mutate(
    se = se_preds,
    lower = pred_height - 1.96 * se,
    upper = pred_height + 1.96 * se
  ) %>%
  add_slope_linetype(
    model = mod_2_alt,
    x_var = "distance_to_edge_m"
  )

# plot
height_distedge <-

ggplot() +
geom_point(data = myc_2023,
           aes(x = distance_to_edge_m,
               y = height_change,
           color = myc_type,
           fill = myc_type ) ) +
  geom_line(data = predicted_data,
            aes(
              x = distance_to_edge_m,
              y = pred_height,
              color = factor(myc_type),
              linetype = slope_linetype),
              linewidth = 2 ) +
  geom_ribbon(data = filter(predicted_data, slope_linetype == "Significant"),
        aes(
          x = distance_to_edge_m,
          y = pred_height,
          ymin = lower, 
          ymax = upper,
          fill = factor(myc_type),
          group = interaction(myc_type, mycorrhizal_legacy)),
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
    y = "Predicted seedling height (cm)",
    x = "Distance from forest
    edge (m)",
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
    data = tibble(
      mycorrhizal_legacy = c("am", "ecm"),
      panel_label = c("A", "B"),
      x = min(myc_2023$distance_to_edge_m),
      y = max(predicted_data$upper - 1)
    ),
    aes(x = x, y = y, label = panel_label),
    hjust = 0,
    size = 4,
    inherit.aes = FALSE
  ) +
  my_theme()

print(height_distedge)


# Figure 2: Foliar 15N enrichment ~ distance to edge ----------------------

mod_foliar15n_distedge <-
  lme4::lmer(
    foliar_15n_enrichment ~
      leaf_percent_n +
      distance_to_edge_m * myc_type * mycorrhizal_legacy +
      (1 | site_unit) + (1 | species),
    data = myc_2023
  )

pred_foliar15n_distedge <- expand_grid(
  distance_to_edge_m = seq(
    min(myc_2023$distance_to_edge_m, na.rm = TRUE),
    max(myc_2023$distance_to_edge_m, na.rm = TRUE),
    length.out = 100
  ),
  myc_type = unique(myc_2023$myc_type),
  mycorrhizal_legacy = unique(myc_2023$mycorrhizal_legacy)
) %>%
  mutate(
    leaf_percent_n = mean(myc_2023$leaf_percent_n, na.rm = TRUE)
  ) %>%
  add_fixed_effect_ci(
    model = mod_foliar15n_distedge,
    fixed_formula = ~ leaf_percent_n +
      distance_to_edge_m * myc_type * mycorrhizal_legacy,
    pred_name = "pred_foliar_15n"
  ) %>%
  add_slope_linetype(
    model = mod_foliar15n_distedge,
    x_var = "distance_to_edge_m"
  )

fig_2_foliar15n_distedge <- model_line_plot(
  raw_data = myc_2023,
  predicted_data = pred_foliar15n_distedge,
  x_var = "distance_to_edge_m",
  y_var = "foliar_15n_enrichment",
  pred_var = "pred_foliar_15n",
  x_label = "Distance from forest edge (m)",
  y_label = "Predicted foliar 15N enrichment"
)

print(fig_2_foliar15n_distedge)


# Figure 3: Foliar 15N enrichment ~ foliar percent N ----------------------

mod_foliar15n_leafn <-
  lme4::lmer(
    foliar_15n_enrichment ~
      leaf_percent_n * myc_type * mycorrhizal_legacy +
      distance_to_edge_m +
      (1 | site_unit) + (1 | species),
    data = myc_2023
  )

pred_foliar15n_leafn <- expand_grid(
  leaf_percent_n = seq(
    min(myc_2023$leaf_percent_n, na.rm = TRUE),
    max(myc_2023$leaf_percent_n, na.rm = TRUE),
    length.out = 100
  ),
  myc_type = unique(myc_2023$myc_type),
  mycorrhizal_legacy = unique(myc_2023$mycorrhizal_legacy)
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


# Figure 4: Foliar percent N ~ distance to edge ---------------------------

mod_leafn_distedge <-
  lme4::lmer(
    leaf_percent_n ~
      distance_to_edge_m * myc_type * mycorrhizal_legacy +
      (1 | condition) + (1 | site_unit) + (1 | species),
    data = myc_2023
  )

pred_leafn_distedge <- expand_grid(
  distance_to_edge_m = seq(
    min(myc_2023$distance_to_edge_m, na.rm = TRUE),
    max(myc_2023$distance_to_edge_m, na.rm = TRUE),
    length.out = 100
  ),
  myc_type = unique(myc_2023$myc_type),
  mycorrhizal_legacy = unique(myc_2023$mycorrhizal_legacy)
) %>%
  add_fixed_effect_ci(
    model = mod_leafn_distedge,
    fixed_formula = ~ distance_to_edge_m * myc_type * mycorrhizal_legacy,
    pred_name = "pred_leaf_percent_n"
  ) %>%
  add_slope_linetype(
    model = mod_leafn_distedge,
    x_var = "distance_to_edge_m"
  )

fig_4_leafn_distedge <- model_line_plot(
  raw_data = myc_2023,
  predicted_data = pred_leafn_distedge,
  x_var = "distance_to_edge_m",
  y_var = "leaf_percent_n",
  pred_var = "pred_leaf_percent_n",
  x_label = "Distance from forest edge (m)",
  y_label = "Predicted leaf percent N"
)

print(fig_4_leafn_distedge)


# Figure 5: Height change ~ foliar percent N ------------------------------

pred_height_leafn <- expand_grid(
  leaf_percent_n = seq(
    min(myc_2023$leaf_percent_n, na.rm = TRUE),
    max(myc_2023$leaf_percent_n, na.rm = TRUE),
    length.out = 100
  ),
  myc_type = unique(myc_2023$myc_type),
  mycorrhizal_legacy = unique(myc_2023$mycorrhizal_legacy)
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


# Export model statistics -------------------------------------------------

final_figure_model_stats <- bind_rows(
  collect_model_stats(
    model = mod_2_alt,
    figure = "Height change ~ distance to edge",
    response = "height_change",
    predictor = "distance_to_edge_m",
    x_var = "distance_to_edge_m"
  ),
  collect_model_stats(
    model = mod_foliar15n_distedge,
    figure = "Figure 2",
    response = "foliar_15n_enrichment",
    predictor = "distance_to_edge_m",
    x_var = "distance_to_edge_m"
  ),
  collect_model_stats(
    model = mod_foliar15n_leafn,
    figure = "Figure 3",
    response = "foliar_15n_enrichment",
    predictor = "leaf_percent_n",
    x_var = "leaf_percent_n"
  ),
  collect_model_stats(
    model = mod_leafn_distedge,
    figure = "Figure 4",
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
  )
)

dir.create("outplut", showWarnings = FALSE, recursive = TRUE)
write_csv(
  final_figure_model_stats,
  "outplut/final_figure_model_stats.csv"
)
