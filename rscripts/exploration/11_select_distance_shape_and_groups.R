# Prespecified distance-response selection workflow
#
# 1. Compare a common linear response with a common quadratic response.
# 2. Using the selected basis, test whether response shape differs by
#    seedling mycorrhizal type and/or site legacy.
# 3. If both modifiers matter, test additive modification vs a full
#    myc-type x legacy interaction in response shape.
# 4. Refit the selected fixed-effect structure with REML.
# 5. Plot population-level fitted relationships over deduplicated raw data.

library(tidyverse)
library(lme4)
library(lmerTest)

alpha <- 0.05
output_dir <- "outplut/distance_shape_selection"
figure_dir <- file.path(output_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Data --------------------------------------------------------------------

dat <- read_csv("outplut/alldata_03262026.csv", show_col_types = FALSE) %>%
  distinct() %>%
  filter(year == "2023") %>%
  mutate(
    myc_type = factor(myc_type, levels = c("am", "ecm")),
    mycorrhizal_legacy = factor(mycorrhizal_legacy, levels = c("am", "ecm")),
    site_unit = factor(site_unit),
    species = factor(species),
    condition = factor(condition),
    distance_center = mean(distance_to_edge_m, na.rm = TRUE),
    dist_c = distance_to_edge_m - distance_center,
    dist_c2 = dist_c^2
  ) %>%
  droplevels()

specs <- list(
  height_change = list(
    label = "Height change (cm)",
    covariates = c(
      "leaf_percent_n * myc_type",
      "foliar_15n_enrichment * myc_type"
    ),
    prediction_covariates = c("leaf_percent_n", "foliar_15n_enrichment"),
    random_terms = c("(1 | condition)", "(1 | site_unit)", "(1 | species)"),
    random_vars = c("condition", "site_unit", "species")
  ),
  foliar_15n_enrichment = list(
    label = "Foliar delta-15N enrichment (per mille)",
    covariates = "leaf_percent_n",
    prediction_covariates = "leaf_percent_n",
    random_terms = c("(1 | site_unit)", "(1 | species)"),
    random_vars = c("site_unit", "species")
  ),
  leaf_percent_n = list(
    label = "Leaf N (%)",
    covariates = character(0),
    prediction_covariates = character(0),
    random_terms = c("(1 | condition)", "(1 | site_unit)", "(1 | species)"),
    random_vars = c("condition", "site_unit", "species")
  )
)

# Formula construction ----------------------------------------------------

distance_terms <- function(shape, modifier = "common") {
  basis <- if (shape == "linear") "dist_c" else "dist_c + dist_c2"
  switch(
    modifier,
    common = basis,
    myc = paste0("(", basis, ") * myc_type"),
    legacy = paste0("(", basis, ") * mycorrhizal_legacy"),
    additive = paste0(
      "(", basis, ") * myc_type + (", basis,
      ") * mycorrhizal_legacy"
    ),
    full = paste0("(", basis, ") * myc_type * mycorrhizal_legacy")
  )
}

make_formula <- function(response, spec, shape, modifier = "common") {
  # myc_type * legacy is retained in all candidates so tests of distance
  # shape do not remove average differences among experimental groups.
  fixed <- c(
    spec$covariates,
    "myc_type * mycorrhizal_legacy",
    distance_terms(shape, modifier)
  )
  as.formula(paste(
    response, "~", paste(c(fixed, spec$random_terms), collapse = " + ")
  ))
}

make_no_distance_formula <- function(response, spec) {
  fixed <- c(spec$covariates, "myc_type * mycorrhizal_legacy")
  as.formula(paste(
    response, "~", paste(c(fixed, spec$random_terms), collapse = " + ")
  ))
}

fit_ml <- function(formula, data) {
  lmer(
    formula, data = data, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

fit_reml <- function(formula, data) {
  lmer(
    formula, data = data, REML = TRUE,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

compare_models <- function(response, question, smaller_name, larger_name, smaller, larger) {
  tab <- as.data.frame(anova(smaller, larger, refit = FALSE))
  p_col <- grep("^Pr\\(", names(tab), value = TRUE)[1]
  chisq_col <- grep("Chisq", names(tab), value = TRUE)[1]
  tibble(
    response = response,
    question = question,
    null_model = smaller_name,
    alternative_model = larger_name,
    chisq = tab[[chisq_col]][2],
    df = attr(logLik(larger), "df") - attr(logLik(smaller), "df"),
    p_value = tab[[p_col]][2],
    AIC_null = AIC(smaller),
    AIC_alternative = AIC(larger),
    delta_AIC_alternative_minus_null = AIC(larger) - AIC(smaller)
  )
}

# Main selection ----------------------------------------------------------

select_model <- function(response, spec, d) {
  tests <- list()

  common_linear <- fit_ml(make_formula(response, spec, "linear", "common"), d)
  common_quadratic <- fit_ml(make_formula(response, spec, "quadratic", "common"), d)
  shape_test <- compare_models(
    response, "Does a common quadratic term improve on a common linear response?",
    "common_linear", "common_quadratic", common_linear, common_quadratic
  )
  tests[[1]] <- shape_test
  shape <- if (shape_test$p_value < alpha) "quadratic" else "linear"
  common <- if (shape == "quadratic") common_quadratic else common_linear

  no_distance <- fit_ml(make_no_distance_formula(response, spec), d)
  distance_test <- compare_models(
    response,
    paste0("Does the selected common ", shape, " distance response improve a model without distance?"),
    "no_distance", paste0("common_", shape), no_distance, common
  )
  tests[[2]] <- distance_test

  by_myc <- fit_ml(make_formula(response, spec, shape, "myc"), d)
  by_legacy <- fit_ml(make_formula(response, spec, shape, "legacy"), d)
  myc_test <- compare_models(
    response, paste0("Does the selected ", shape, " response differ by myc type?"),
    paste0("common_", shape), paste0(shape, "_by_myc"), common, by_myc
  )
  legacy_test <- compare_models(
    response, paste0("Does the selected ", shape, " response differ by legacy?"),
    paste0("common_", shape), paste0(shape, "_by_legacy"), common, by_legacy
  )
  tests[[3]] <- myc_test
  tests[[4]] <- legacy_test

  myc_modifies <- myc_test$p_value < alpha
  legacy_modifies <- legacy_test$p_value < alpha

  if (!myc_modifies && !legacy_modifies) {
    modifier <- "common"
    selected_ml <- common
  } else if (myc_modifies && !legacy_modifies) {
    modifier <- "myc"
    selected_ml <- by_myc
  } else if (!myc_modifies && legacy_modifies) {
    modifier <- "legacy"
    selected_ml <- by_legacy
  } else {
    additive <- fit_ml(make_formula(response, spec, shape, "additive"), d)
    full <- fit_ml(make_formula(response, spec, shape, "full"), d)
    interaction_test <- compare_models(
      response,
      paste0("Does myc-by-legacy interaction further modify the ", shape, " response?"),
      paste0(shape, "_additive_modifiers"), paste0(shape, "_full_interaction"),
      additive, full
    )
    tests[[5]] <- interaction_test
    if (interaction_test$p_value < alpha) {
      modifier <- "full"
      selected_ml <- full
    } else {
      modifier <- "additive"
      selected_ml <- additive
    }
  }

  # After selecting response shape, remove unsupported grouping factors in a
  # hierarchy-respecting way. Removing a factor also removes all terms that
  # contain it. If both are removable, remove the one giving lower AIC first,
  # then retest the other factor against that reduced model.
  reduce_factor <- function(model, factor_name) {
    fixed_formula <- lme4::nobars(formula(model))
    labels <- attr(terms(fixed_formula), "term.labels")
    labels <- labels[!grepl(factor_name, labels, fixed = TRUE)]
    reduced_formula <- as.formula(paste(
      response, "~", paste(c(labels, spec$random_terms), collapse = " + ")
    ))
    fit_ml(reduced_formula, d)
  }

  group_tests <- list()
  myc_reduced <- reduce_factor(selected_ml, "myc_type")
  legacy_reduced <- reduce_factor(selected_ml, "mycorrhizal_legacy")
  myc_overall <- compare_models(
    response, "Does myc_type contribute overall?",
    "without_myc_type", "current_model", myc_reduced, selected_ml
  )
  legacy_overall <- compare_models(
    response, "Does mycorrhizal_legacy contribute overall?",
    "without_mycorrhizal_legacy", "current_model", legacy_reduced, selected_ml
  )
  group_tests[[1]] <- myc_overall
  group_tests[[2]] <- legacy_overall

  current_ml <- selected_ml
  removed_groups <- character(0)
  removable <- bind_rows(myc_overall, legacy_overall) %>% filter(p_value >= alpha)
  if (nrow(removable) > 0) {
    first_factor <- if (
      AIC(myc_reduced) <= AIC(legacy_reduced) && myc_overall$p_value >= alpha
    ) "myc_type" else "mycorrhizal_legacy"
    current_ml <- if (first_factor == "myc_type") myc_reduced else legacy_reduced
    removed_groups <- first_factor

    second_factor <- setdiff(c("myc_type", "mycorrhizal_legacy"), first_factor)
    second_reduced <- reduce_factor(current_ml, second_factor)
    second_test <- compare_models(
      response, paste0("After removing ", first_factor, ", does ", second_factor, " contribute overall?"),
      paste0("without_", first_factor, "_and_", second_factor),
      paste0("without_", first_factor), second_reduced, current_ml
    )
    group_tests[[3]] <- second_test
    if (second_test$p_value >= alpha) {
      current_ml <- second_reduced
      removed_groups <- c(removed_groups, second_factor)
    }
  }

  final_formula <- formula(current_ml)
  final_reml <- fit_reml(final_formula, d)

  list(
    shape = shape,
    modifier = modifier,
    tests = bind_rows(tests),
    selected_ml = current_ml,
    final_reml = final_reml,
    formula = final_formula,
    group_tests = bind_rows(group_tests),
    removed_groups = removed_groups
  )
}

# Group-level omnibus tests ----------------------------------------------
# These are descriptive follow-up tests of whether myc type or legacy makes
# any contribution at all (average level and any retained shape terms).

# Predictions and plotting ------------------------------------------------

prediction_grid <- function(d, spec, model, length_out = 150) {
  grid <- d %>%
    group_by(myc_type, mycorrhizal_legacy) %>%
    summarise(
      distance_to_edge_m = list(seq(
        min(distance_to_edge_m), max(distance_to_edge_m), length.out = length_out
      )),
      .groups = "drop"
    ) %>%
    unnest(distance_to_edge_m) %>%
    mutate(
      distance_center = unique(d$distance_center),
      dist_c = distance_to_edge_m - distance_center,
      dist_c2 = dist_c^2
    )

  for (v in spec$prediction_covariates) grid[[v]] <- mean(d[[v]], na.rm = TRUE)
  for (v in spec$random_vars) {
    if (!v %in% names(grid)) {
      grid[[v]] <- factor(levels(d[[v]])[1], levels = levels(d[[v]]))
    }
  }

  fixed_formula <- delete.response(terms(lme4::nobars(formula(model))))
  x <- model.matrix(fixed_formula, grid)
  beta <- fixef(model)
  x <- x[, names(beta), drop = FALSE]
  fit <- as.numeric(x %*% beta)
  se <- sqrt(diag(x %*% vcov(model) %*% t(x)))
  grid %>% mutate(fit = fit, se = se, lower = fit - 1.96 * se, upper = fit + 1.96 * se)
}

make_plot <- function(response, spec, d, pred, selection) {
  base <- ggplot() +
    geom_point(
      data = d,
      aes(x = distance_to_edge_m, y = .data[[response]], color = myc_type),
      alpha = 0.55
    )

  if ("myc_type" %in% selection$removed_groups) {
    pred_lines <- pred %>% distinct(mycorrhizal_legacy, distance_to_edge_m, .keep_all = TRUE)
    base <- base +
      geom_ribbon(
        data = pred_lines,
        aes(x = distance_to_edge_m, ymin = lower, ymax = upper, group = mycorrhizal_legacy),
        fill = "grey60", alpha = 0.2
      ) +
      geom_line(
        data = pred_lines,
        aes(x = distance_to_edge_m, y = fit, group = mycorrhizal_legacy),
        color = "black", linewidth = 1.3
      )
  } else {
    base <- base +
      geom_ribbon(
        data = pred,
        aes(
          x = distance_to_edge_m, ymin = lower, ymax = upper,
          fill = myc_type, group = interaction(myc_type, mycorrhizal_legacy)
        ),
        alpha = 0.18
      ) +
      geom_line(
        data = pred,
        aes(x = distance_to_edge_m, y = fit, color = myc_type),
        linewidth = 1.3
      )
  }

  base +
    facet_wrap(
      ~mycorrhizal_legacy,
      labeller = as_labeller(c(am = "AM legacy plot", ecm = "EcM legacy plot"))
    ) +
    scale_color_manual(values = c(am = "#4292C6", ecm = "#F16913")) +
    scale_fill_manual(values = c(am = "#4292C6", ecm = "#F16913"), guide = "none") +
    labs(
      x = "Distance from forest edge (m)", y = spec$label,
      color = "Seedling mycorrhizal type",
      title = paste(spec$label, "versus distance to edge"),
      subtitle = paste0(
        "Selected shape: ", selection$shape,
        "; shape modifier: ", selection$modifier,
        "; population-level 95% CI"
      )
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom", plot.title.position = "plot")
}

# Run ---------------------------------------------------------------------

all_tests <- list()
all_group_tests <- list()
all_decisions <- list()
all_fixed <- list()

for (response in names(specs)) {
  spec <- specs[[response]]
  needed <- unique(c(
    response, "distance_to_edge_m", "dist_c", "dist_c2",
    "myc_type", "mycorrhizal_legacy",
    spec$prediction_covariates, spec$random_vars
  ))
  d <- dat %>% filter(if_all(all_of(needed), ~ !is.na(.x))) %>% droplevels()

  selection <- select_model(response, spec, d)
  group_tests <- selection$group_tests
  pred <- prediction_grid(d, spec, selection$final_reml)
  fig <- make_plot(response, spec, d, pred, selection)

  fixed <- as.data.frame(coef(summary(selection$final_reml)))
  fixed$term <- rownames(fixed)
  rownames(fixed) <- NULL
  fixed$response <- response

  all_tests[[response]] <- selection$tests
  all_group_tests[[response]] <- group_tests
  all_decisions[[response]] <- tibble(
    response = response,
    n = nrow(d),
    selected_shape = selection$shape,
    selected_shape_modifier = selection$modifier,
    removed_group_factors = paste(selection$removed_groups, collapse = ";"),
    selected_formula = paste(deparse(selection$formula), collapse = " "),
    ML_AIC = AIC(selection$selected_ml),
    ML_BIC = BIC(selection$selected_ml),
    REML_singular = isSingular(selection$final_reml)
  )
  all_fixed[[response]] <- fixed

  write_csv(pred, file.path(output_dir, paste0(response, "_prediction_grid.csv")))
  capture.output(summary(selection$final_reml), file = file.path(output_dir, paste0(response, "_model_summary.txt")))
  ggsave(
    file.path(figure_dir, paste0(response, "_selected_distance_model.png")),
    fig, width = 8, height = 6, dpi = 300, bg = "white"
  )
}

write_csv(bind_rows(all_tests), file.path(output_dir, "sequential_model_tests.csv"))
write_csv(bind_rows(all_group_tests), file.path(output_dir, "overall_group_tests.csv"))
write_csv(bind_rows(all_decisions), file.path(output_dir, "selected_models.csv"))
write_csv(bind_rows(all_fixed), file.path(output_dir, "selected_model_fixed_effects.csv"))

cat("\nSelected models:\n")
print(bind_rows(all_decisions))
cat("\nSequential tests:\n")
print(bind_rows(all_tests))
cat("\nOverall group tests:\n")
print(bind_rows(all_group_tests))
