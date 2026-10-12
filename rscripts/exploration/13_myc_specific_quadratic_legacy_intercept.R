# Focused test: does distance curvature differ by seedling mycorrhizal type?
# Site legacy is retained as an intercept-level fixed effect only.
#
# PLAIN-LANGUAGE OVERVIEW
# -----------------------
# This script compares three models:
#   1. AM and EcM have different straight-line distance slopes.
#   2. AM and EcM have different slopes but share one quadratic curvature.
#   3. AM and EcM have different slopes AND different curvature.
#
# Legacy remains a main effect in every model. It may shift the response up
# or down, but it is not allowed to change the distance slope or curvature.
# Fixed-effect comparisons use maximum likelihood (REML = FALSE). The full
# myc-specific quadratic candidate is refitted with REML only for its plotted
# estimates; that refit does not mean the candidate was selected.

library(tidyverse)
library(lme4)
library(lmerTest)

output_dir <- "outplut/myc_specific_quadratic_legacy_intercept"
figure_dir <- file.path(output_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# The source table contains every 2023 record twice. distinct() removes those
# exact duplicates so n, uncertainty, and p-values are not artificially high.
dat <- read_csv("outplut/alldata_03262026.csv", show_col_types = FALSE) %>%
  distinct() %>%
  filter(year == "2023") %>%
  mutate(
    myc_type = factor(myc_type, levels = c("am", "ecm")),
    mycorrhizal_legacy = factor(mycorrhizal_legacy, levels = c("am", "ecm")),
    condition = factor(condition),
    site_unit = factor(site_unit),
    species = factor(species),
    # Centering reduces correlation between distance and distance squared.
    # It also makes the linear coefficient the slope at the mean distance.
    distance_center = mean(distance_to_edge_m, na.rm = TRUE),
    dist_c = distance_to_edge_m - distance_center,
    dist_c2 = dist_c^2
  ) %>%
  droplevels()

# Response-specific covariates and random effects match the manuscript models.
# prediction_covariates are held at their means when model lines are drawn.
specs <- list(
  height_change = list(
    label = "Height change (cm)",
    covariates = c(
      "leaf_percent_n * myc_type",
      "foliar_15n_enrichment * myc_type"
    ),
    required = c("leaf_percent_n", "foliar_15n_enrichment"),
    prediction_covariates = c("leaf_percent_n", "foliar_15n_enrichment"),
    random = c("(1 | condition)", "(1 | site_unit)", "(1 | species)"),
    random_vars = c("condition", "site_unit", "species")
  ),
  foliar_15n_enrichment = list(
    label = "Foliar delta-15N enrichment (per mille)",
    covariates = "leaf_percent_n",
    required = "leaf_percent_n",
    prediction_covariates = "leaf_percent_n",
    random = c("(1 | site_unit)", "(1 | species)"),
    random_vars = c("site_unit", "species")
  ),
  leaf_percent_n = list(
    label = "Leaf N (%)",
    covariates = character(0),
    required = character(0),
    prediction_covariates = character(0),
    random = c("(1 | condition)", "(1 | site_unit)", "(1 | species)"),
    random_vars = c("condition", "site_unit", "species")
  )
)

make_formula <- function(response, spec, structure) {
  # linear_myc expands to dist_c + myc_type + dist_c:myc_type.
  # It allows different AM/EcM straight-line slopes but no curvature.
  #
  # common_quadratic adds one dist_c2 coefficient shared by AM and EcM.
  #
  # myc_quadratic also adds dist_c2:myc_type, allowing AM and EcM to
  # have different quadratic coefficients.
  distance <- switch(
    structure,
    linear_myc = "dist_c * myc_type",
    common_quadratic = "dist_c * myc_type + dist_c2",
    myc_quadratic = "(dist_c + dist_c2) * myc_type"
  )
  # Legacy is outside the distance interaction: intercept effect only.
  as.formula(paste(
    response, "~",
    paste(c(spec$covariates, "mycorrhizal_legacy", distance, spec$random), collapse = " + ")
  ))
}

fit_model <- function(formula, data, REML = FALSE) {
  # Use ML when comparing fixed effects. REML is reserved for the final
  # coefficient/prediction refit after a structure has been chosen.
  lme4::lmer(
    formula, data = data, REML = REML,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

compare_models <- function(response, question, smaller_name, larger_name, smaller, larger) {
  # Nested likelihood-ratio test:
  # H0 = the smaller model is sufficient.
  # H1 = the added term(s) in the larger model improve fit.
  # p < 0.05 supports the larger model. Negative delta_AIC also favors it.
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
    delta_AIC = AIC(larger) - AIC(smaller),
    BIC_null = BIC(smaller),
    BIC_alternative = BIC(larger)
  )
}

curvature_contrasts <- function(model, response) {
  # The coefficient table prints AM curvature directly because AM is the
  # reference group. EcM curvature is the linear combination:
  #   dist_c2 + dist_c2:myc_typeecm
  # This function calculates both direct group estimates and their uncertainty.
  # A significant EcM contrast and nonsignificant AM contrast do NOT by
  # themselves prove a difference; that requires the interaction test below.
  beta <- fixef(model)
  vc <- vcov(model)
  mf <- model.frame(model)
  fixed_formula <- delete.response(terms(lme4::nobars(formula(model))))

  bind_rows(lapply(c("am", "ecm"), function(mt) {
    nd <- mf[rep(1, 2), , drop = FALSE]
    # Change only dist_c2 from 0 to 1 in the model matrix, thereby isolating
    # the quadratic coefficient for the requested mycorrhizal type.
    nd$dist_c <- 0
    nd$dist_c2 <- c(0, 1)
    nd$myc_type <- factor(mt, levels = levels(mf$myc_type))
    mm <- model.matrix(fixed_formula, nd)[, names(beta), drop = FALSE]
    contrast <- mm[2, ] - mm[1, ]
    estimate <- sum(contrast * beta)
    se <- sqrt(as.numeric(t(contrast) %*% vc %*% contrast))
    z <- estimate / se
    tibble(
      response = response, myc_type = mt,
      quadratic_estimate = estimate, std_error = se,
      z_value = z,
      p_value = 2 * pnorm(abs(z), lower.tail = FALSE),
      conf_low = estimate - 1.96 * se,
      conf_high = estimate + 1.96 * se
    )
  }))
}

prediction_grid <- function(d, spec, model) {
  # Create evenly spaced distances for each myc type x legacy combination.
  # Continuous covariates are held at their means. Predictions use only fixed
  # effects, so the lines represent population-level relationships.
  grid <- expand_grid(
    myc_type = factor(c("am", "ecm"), levels = levels(d$myc_type)),
    mycorrhizal_legacy = factor(c("am", "ecm"), levels = levels(d$mycorrhizal_legacy)),
    distance_to_edge_m = seq(
      min(d$distance_to_edge_m), max(d$distance_to_edge_m), length.out = 150
    )
  ) %>%
    mutate(
      distance_center = unique(d$distance_center),
      dist_c = distance_to_edge_m - distance_center,
      dist_c2 = dist_c^2
    )
  for (v in spec$prediction_covariates) grid[[v]] <- mean(d[[v]], na.rm = TRUE)
  for (v in spec$random_vars) {
    if (!v %in% names(grid)) grid[[v]] <- factor(levels(d[[v]])[1], levels = levels(d[[v]]))
  }
  ff <- delete.response(terms(lme4::nobars(formula(model))))
  x <- model.matrix(ff, grid)
  beta <- fixef(model)
  x <- x[, names(beta), drop = FALSE]
  fit <- as.numeric(x %*% beta)
  se <- sqrt(diag(x %*% vcov(model) %*% t(x)))
  grid %>% mutate(fit = fit, se = se, lower = fit - 1.96 * se, upper = fit + 1.96 * se)
}

make_plot <- function(response, spec, d, pred) {
  # Points are raw deduplicated observations. Lines and ribbons are model
  # predictions and approximate fixed-effect 95% confidence intervals.
  ggplot() +
    geom_point(
      data = d,
      aes(distance_to_edge_m, .data[[response]], color = myc_type),
      alpha = 0.5
    ) +
    geom_ribbon(
      data = pred,
      aes(
        distance_to_edge_m, ymin = lower, ymax = upper,
        fill = myc_type, group = interaction(myc_type, mycorrhizal_legacy)
      ), alpha = 0.18
    ) +
    geom_line(
      data = pred,
      aes(distance_to_edge_m, fit, color = myc_type),
      linewidth = 1.3
    ) +
    facet_wrap(
      ~mycorrhizal_legacy,
      labeller = as_labeller(c(am = "AM legacy plot", ecm = "EcM legacy plot"))
    ) +
    scale_color_manual(values = c(am = "#4292C6", ecm = "#F16913")) +
    scale_fill_manual(values = c(am = "#4292C6", ecm = "#F16913"), guide = "none") +
    labs(
      x = "Distance from forest edge (m)", y = spec$label,
      color = "Seedling mycorrhizal type",
      title = paste(spec$label, "with myc-specific quadratic curves"),
      subtitle = "Legacy retained as an intercept effect; population-level 95% CI"
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom", plot.title.position = "plot")
}

tests <- list()
contrasts <- list()
model_info <- list()

for (response in names(specs)) {
  message("Testing ", response, "...")
  spec <- specs[[response]]
  needed <- unique(c(
    response, "distance_to_edge_m", "dist_c", "dist_c2",
    "myc_type", "mycorrhizal_legacy", spec$required, spec$random_vars
  ))
  d <- dat %>% filter(if_all(all_of(needed), ~ !is.na(.x))) %>% droplevels()

  # Fit all candidates to exactly the same complete-case rows.
  linear <- fit_model(make_formula(response, spec, "linear_myc"), d)
  common_quad <- fit_model(make_formula(response, spec, "common_quadratic"), d)
  myc_quad <- fit_model(make_formula(response, spec, "myc_quadratic"), d)

  tests[[response]] <- bind_rows(
    # TEST 1 (2 df): Does adding both the shared x^2 term and the
    # x^2-by-myc interaction improve myc-specific straight lines?
    # This is the main omnibus test for myc-specific curvature.
    compare_models(
      response, "Does adding AM- and EcM-specific curvature improve myc-specific linear slopes?",
      "myc_specific_linear", "myc_specific_quadratic", linear, myc_quad
    ),
    # TEST 2 (1 df): With a quadratic term in both models, is the additional
    # x^2-by-myc interaction needed? This formally tests whether AM and EcM
    # curvature differ.
    compare_models(
      response, "Does curvature differ between AM and EcM?",
      "common_quadratic", "myc_specific_quadratic", common_quad, myc_quad
    ),
    # TEST 3 (1 df): After allowing different AM/EcM linear slopes, does one
    # shared quadratic term improve the model?
    compare_models(
      response, "Does a common quadratic term improve myc-specific linear slopes?",
      "myc_specific_linear", "common_quadratic", linear, common_quad
    )
  )
  # Follow-up within-type estimates. Interpret cautiously when Test 1 or
  # Test 2 is nonsignificant; do not select a model from one contrast alone.
  contrasts[[response]] <- curvature_contrasts(myc_quad, response)

  model_info[[response]] <- tibble(
    response = response, n = nrow(d),
    linear_AIC = AIC(linear), common_quadratic_AIC = AIC(common_quad),
    myc_quadratic_AIC = AIC(myc_quad),
    linear_BIC = BIC(linear), common_quadratic_BIC = BIC(common_quad),
    myc_quadratic_BIC = BIC(myc_quad),
    linear_singular = isSingular(linear),
    common_quadratic_singular = isSingular(common_quad),
    myc_quadratic_singular = isSingular(myc_quad)
  )

  # Exploratory REML refit used for the myc-specific quadratic figures.
  # The model comparison above—not this refit—determines model support.
  final_quad <- fit_model(make_formula(response, spec, "myc_quadratic"), d, REML = TRUE)
  pred <- prediction_grid(d, spec, final_quad)
  fig <- make_plot(response, spec, d, pred)
  write_csv(pred, file.path(output_dir, paste0(response, "_myc_quadratic_prediction_grid.csv")))
  ggsave(
    file.path(figure_dir, paste0(response, "_myc_quadratic.png")),
    fig, width = 8, height = 6, dpi = 300, bg = "white"
  )
  capture.output(summary(final_quad), file = file.path(output_dir, paste0(response, "_myc_quadratic_summary.txt")))
}

write_csv(bind_rows(tests), file.path(output_dir, "focused_model_tests.csv"))
write_csv(bind_rows(contrasts), file.path(output_dir, "am_ecm_curvature_contrasts.csv"))
write_csv(bind_rows(model_info), file.path(output_dir, "candidate_model_information.csv"))

cat("\nFocused model tests:\n")
print(bind_rows(tests))
cat("\nAM and EcM curvature contrasts:\n")
print(bind_rows(contrasts))
