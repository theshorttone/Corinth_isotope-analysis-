# Quadratic extension of the focal distance-to-edge analyses in
# rscripts/05_final_figures.R
#
# Responses included:
#   1. Height change
#   2. Foliar delta-15N enrichment
#   3. Leaf percent N
#
# The original script is not modified. This version:
#   * removes exact duplicate 2023 records;
#   * centers distance before calculating distance squared;
#   * preserves the original linear distance x myc type x legacy interaction;
#   * begins with four myc-type x legacy-specific quadratic responses;
#   * simplifies only the quadratic component using nested ML tests;
#   * preserves condition as a random intercept where it occurred originally;
#   * refits the selected model using REML for final inference and figures.

library(tidyverse)
library(lme4)
library(lmerTest)
library(RColorBrewer)

alpha_quadratic <- 0.05
output_dir <- "outplut/quadratic_final_figures"
figure_dir <- file.path(output_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Data --------------------------------------------------------------------

myc_alldata <-
  read_csv("outplut/alldata_03262026.csv", show_col_types = FALSE) %>%
  distinct() %>%
  mutate(
    site_unit = factor(site_unit),
    species = factor(species),
    condition = factor(condition)
  )

myc_2023 <-
  myc_alldata %>%
  filter(year == "2023") %>%
  filter(
    !is.na(distance_to_edge_m),
    !is.na(myc_type),
    !is.na(mycorrhizal_legacy)
  ) %>%
  droplevels() %>%
  mutate(
    myc_type = factor(myc_type, levels = c("am", "ecm")),
    mycorrhizal_legacy = factor(mycorrhizal_legacy, levels = c("am", "ecm")),
    dist_center = mean(distance_to_edge_m, na.rm = TRUE),
    dist_c = distance_to_edge_m - dist_center,
    dist_c2 = dist_c^2
  )

# Theme -------------------------------------------------------------------

blues <- RColorBrewer::brewer.pal(9, "Blues")
oranges <- RColorBrewer::brewer.pal(9, "Oranges")
myc_association_colors <- c("am" = blues[5], "ecm" = oranges[5])

my_theme <- function() {
  theme_classic() +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      strip.text = element_text(size = 8),
      legend.position = "bottom"
    )
}

# Candidate quadratic structures -----------------------------------------
#
# The linear portion remains identical in every candidate:
#   dist_c * myc_type * mycorrhizal_legacy
#
# Only the quadratic portion is simplified:
#   full       = curvature differs by myc type AND legacy (four curves)
#   by_myc     = curvature differs only by myc type
#   by_legacy  = curvature differs only by legacy
#   common     = one common curvature for all groups
#   none       = no quadratic curvature (original linear structure)

quadratic_terms <- c(
  full = "dist_c2 * myc_type * mycorrhizal_legacy",
  by_myc = "dist_c2 * myc_type",
  by_legacy = "dist_c2 * mycorrhizal_legacy",
  common = "dist_c2",
  none = ""
)

build_formula <- function(response, covariates, random_effects, quadratic_structure) {
  fixed_terms <- c(
    covariates,
    "dist_c * myc_type * mycorrhizal_legacy",
    quadratic_terms[[quadratic_structure]]
  )
  fixed_terms <- fixed_terms[nzchar(fixed_terms)]
  as.formula(paste(
    response, "~", paste(c(fixed_terms, random_effects), collapse = " + ")
  ))
}

fit_candidate_set <- function(response, covariates, random_effects, data) {
  lapply(names(quadratic_terms), function(structure) {
    lmer(
      build_formula(response, covariates, random_effects, structure),
      data = data,
      REML = FALSE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
  }) %>%
    setNames(names(quadratic_terms))
}

lrt_row <- function(response, larger_name, smaller_name, larger, smaller) {
  test <- as.data.frame(anova(smaller, larger, refit = FALSE))
  p_column <- grep("^Pr\\(", names(test), value = TRUE)[1]
  chisq_column <- grep("Chisq", names(test), value = TRUE)[1]
  df_difference <- attr(logLik(larger), "df") - attr(logLik(smaller), "df")
  data.frame(
    response = response,
    larger_model = larger_name,
    smaller_model = smaller_name,
    chisq = test[[chisq_column]][2],
    df = df_difference,
    p_value = test[[p_column]][2],
    AIC_larger = AIC(larger),
    AIC_smaller = AIC(smaller)
  )
}

select_quadratic_structure <- function(response, candidates, alpha = 0.05) {
  tests <- bind_rows(
    lrt_row(response, "full", "by_myc", candidates$full, candidates$by_myc),
    lrt_row(response, "full", "by_legacy", candidates$full, candidates$by_legacy)
  )

  # If either reduced model is not significantly worse than the full model,
  # retain the reduced option with the lower AIC. If both are significantly
  # worse, retain four group-specific quadratic curves.
  eligible_first <- tests %>% filter(p_value >= alpha)
  if (nrow(eligible_first) == 0) {
    return(list(selected = "full", tests = tests))
  }

  current <- eligible_first %>%
    arrange(AIC_smaller) %>%
    slice(1) %>%
    pull(smaller_model)

  # Test whether the retained two-level curvature can be reduced to a common
  # quadratic term.
  common_test <- lrt_row(
    response, current, "common", candidates[[current]], candidates$common
  )
  tests <- bind_rows(tests, common_test)

  if (common_test$p_value >= alpha) {
    current <- "common"

    # Finally test whether any quadratic term is needed.
    none_test <- lrt_row(
      response, "common", "none", candidates$common, candidates$none
    )
    tests <- bind_rows(tests, none_test)
    if (none_test$p_value >= alpha) current <- "none"
  }

  list(selected = current, tests = tests)
}

# Prediction helpers ------------------------------------------------------

group_prediction_grid <- function(data, response, length_out = 150) {
  data %>%
    filter(!is.na(.data[[response]])) %>%
    group_by(myc_type, mycorrhizal_legacy) %>%
    summarise(
      distance_to_edge_m = list(seq(
        min(distance_to_edge_m), max(distance_to_edge_m), length.out = length_out
      )),
      .groups = "drop"
    ) %>%
    unnest(distance_to_edge_m) %>%
    mutate(
      dist_center = unique(data$dist_center),
      dist_c = distance_to_edge_m - dist_center,
      dist_c2 = dist_c^2
    )
}

add_reference_values <- function(grid, model_data, covariates, random_variables) {
  for (v in covariates) {
    if (!v %in% names(grid)) grid[[v]] <- mean(model_data[[v]], na.rm = TRUE)
  }
  for (v in random_variables) {
    if (!v %in% names(grid)) {
      grid[[v]] <- factor(levels(model_data[[v]])[1], levels = levels(model_data[[v]]))
    }
  }
  grid
}

add_fixed_predictions <- function(grid, model) {
  fixed_formula <- delete.response(terms(lme4::nobars(formula(model))))
  x <- model.matrix(fixed_formula, data = grid)
  beta <- fixef(model)
  x <- x[, names(beta), drop = FALSE]
  fit <- as.numeric(x %*% beta)
  se <- sqrt(diag(x %*% vcov(model) %*% t(x)))
  grid %>% mutate(fit = fit, se = se, lower = fit - 1.96 * se, upper = fit + 1.96 * se)
}

quadratic_plot <- function(raw_data, predictions, response, y_label, title) {
  ggplot() +
    geom_point(
      data = raw_data,
      aes(x = distance_to_edge_m, y = .data[[response]], color = myc_type),
      alpha = 0.6
    ) +
    geom_ribbon(
      data = predictions,
      aes(
        x = distance_to_edge_m, ymin = lower, ymax = upper,
        fill = myc_type, group = interaction(myc_type, mycorrhizal_legacy)
      ),
      alpha = 0.2
    ) +
    geom_line(
      data = predictions,
      aes(x = distance_to_edge_m, y = fit, color = myc_type),
      linewidth = 1.4
    ) +
    facet_wrap(
      ~mycorrhizal_legacy,
      labeller = as_labeller(c(am = "AM legacy plot", ecm = "EcM legacy plot"))
    ) +
    scale_color_manual(values = myc_association_colors, name = "Mycorrhizal association") +
    scale_fill_manual(values = myc_association_colors, guide = "none") +
    labs(
      x = "Distance from forest edge (m)", y = y_label, title = title,
      subtitle = "Selected quadratic structure; population-level predictions with 95% CI"
    ) +
    my_theme()
}

# Response specifications -------------------------------------------------

response_specs <- list(
  height_change = list(
    y_label = "Height change (cm)",
    title = "Height change versus distance to edge",
    covariates = c(
      "leaf_percent_n", "leaf_percent_n:myc_type",
      "foliar_15n_enrichment", "foliar_15n_enrichment:myc_type"
    ),
    prediction_covariates = c("leaf_percent_n", "foliar_15n_enrichment"),
    random_effects = c("(1 | condition)", "(1 | site_unit)", "(1 | species)"),
    random_variables = c("condition", "site_unit", "species")
  ),
  foliar_15n_enrichment = list(
    y_label = expression(paste("Foliar ", delta^{15}, "N enrichment (\u2030)")),
    title = "Foliar delta-15N enrichment versus distance to edge",
    covariates = "leaf_percent_n",
    prediction_covariates = "leaf_percent_n",
    random_effects = c("(1 | site_unit)", "(1 | species)"),
    random_variables = c("site_unit", "species")
  ),
  leaf_percent_n = list(
    y_label = "Leaf N (%)",
    title = "Leaf N versus distance to edge",
    covariates = character(0),
    prediction_covariates = character(0),
    random_effects = c("(1 | condition)", "(1 | site_unit)", "(1 | species)"),
    random_variables = c("condition", "site_unit", "species")
  )
)

# Fit, select, refit, predict, and export ---------------------------------

all_selection_tests <- list()
all_model_comparisons <- list()
all_fixed_effects <- list()
selected_structures <- list()

for (response in names(response_specs)) {
  spec <- response_specs[[response]]

  variables_needed <- unique(c(
    response,
    "distance_to_edge_m", "dist_c", "dist_c2",
    "myc_type", "mycorrhizal_legacy",
    spec$prediction_covariates,
    spec$random_variables
  ))
  model_data <- myc_2023 %>%
    filter(if_all(all_of(variables_needed), ~ !is.na(.x))) %>%
    droplevels()

  candidates <- fit_candidate_set(
    response, spec$covariates, spec$random_effects, model_data
  )
  selection <- select_quadratic_structure(
    response, candidates, alpha = alpha_quadratic
  )
  selected_structures[[response]] <- selection$selected
  all_selection_tests[[response]] <- selection$tests

  all_model_comparisons[[response]] <- tibble(
    response = response,
    quadratic_structure = names(candidates),
    n = nrow(model_data),
    AIC = map_dbl(candidates, AIC),
    BIC = map_dbl(candidates, BIC),
    logLik = map_dbl(candidates, ~ as.numeric(logLik(.x))),
    selected = names(candidates) == selection$selected
  )

  selected_formula <- build_formula(
    response, spec$covariates, spec$random_effects, selection$selected
  )
  final_model <- lmer(
    selected_formula,
    data = model_data,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )

  fixed <- as.data.frame(coef(summary(final_model)))
  fixed$term <- rownames(fixed)
  rownames(fixed) <- NULL
  fixed$response <- response
  fixed$selected_quadratic_structure <- selection$selected
  all_fixed_effects[[response]] <- fixed

  prediction_grid <- group_prediction_grid(model_data, response) %>%
    add_reference_values(
      model_data,
      spec$prediction_covariates,
      spec$random_variables
    ) %>%
    add_fixed_predictions(final_model)

  figure <- quadratic_plot(
    model_data, prediction_grid, response,
    spec$y_label, spec$title
  )

  ggsave(
    file.path(figure_dir, paste0(response, "_selected_quadratic.png")),
    figure, width = 8, height = 6, dpi = 300, bg = "white"
  )

  # Retain an explicitly labelled exploratory height figure for the common
  # quadratic model (linear-vs-common-quadratic ML test p = 0.091). This does
  # not change the automatically selected final height model above.
  if (response == "height_change") {
    exploratory_height_model <- lmer(
      build_formula(
        response, spec$covariates, spec$random_effects,
        quadratic_structure = "common"
      ),
      data = model_data,
      REML = TRUE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
    exploratory_height_predictions <- group_prediction_grid(model_data, response) %>%
      add_reference_values(
        model_data,
        spec$prediction_covariates,
        spec$random_variables
      ) %>%
      add_fixed_predictions(exploratory_height_model)
    exploratory_height_figure <- quadratic_plot(
      model_data,
      exploratory_height_predictions,
      response,
      spec$y_label,
      "Height change versus distance to edge (exploratory common quadratic; p = 0.091)"
    )
    ggsave(
      file.path(figure_dir, "height_change_common_quadratic_p_0.091.png"),
      exploratory_height_figure,
      width = 8, height = 6, dpi = 300, bg = "white"
    )
    write_csv(
      exploratory_height_predictions,
      file.path(output_dir, "height_change_common_quadratic_p_0.091_prediction_grid.csv")
    )
  }
  write_csv(
    prediction_grid,
    file.path(output_dir, paste0(response, "_selected_prediction_grid.csv"))
  )
  capture.output(
    summary(final_model),
    file = file.path(output_dir, paste0(response, "_selected_model_summary.txt"))
  )
}

write_csv(bind_rows(all_selection_tests), file.path(output_dir, "quadratic_structure_tests.csv"))
write_csv(bind_rows(all_model_comparisons), file.path(output_dir, "quadratic_candidate_comparisons.csv"))
write_csv(bind_rows(all_fixed_effects), file.path(output_dir, "selected_model_fixed_effects.csv"))
write_csv(
  enframe(unlist(selected_structures), name = "response", value = "selected_quadratic_structure"),
  file.path(output_dir, "selected_quadratic_structures.csv")
)

cat("\nSelected quadratic structures:\n")
print(unlist(selected_structures))
cat("\nNested likelihood-ratio tests:\n")
print(bind_rows(all_selection_tests))
