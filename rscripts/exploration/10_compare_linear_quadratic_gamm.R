# Compare linear, quadratic, and low-complexity GAMM distance responses
# for height change, foliar delta-15N enrichment, and leaf N.
#
# Workflow:
# 1. Fit a linear mixed model.
# 2. Fit a quadratic mixed model using centered distance.
# 3. Fit a conservative GAMM (k = 4).
# 4. Use identical observations, covariates, and random effects per response.
# 5. Compare ML AIC, nested linear-vs-quadratic LRT, diagnostics, and
#    leave-one-site-unit-out predictive performance.
# 6. Plot raw observations and population-level fits from all three models.
#
# Important: alldata_03262026.csv contains each 2023 record twice. This script
# explicitly removes exact duplicate rows before any model is fitted.

required_packages <- c("mgcv", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install required packages first: ", paste(missing_packages, collapse = ", "))
}

library(mgcv)
library(ggplot2)

input_file <- "outplut/alldata_03262026.csv"
output_dir <- "outplut/model_comparison_linear_quadratic_gamm"
figure_dir <- file.path(output_dir, "figures")
diagnostic_dir <- file.path(output_dir, "diagnostics")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)

raw <- read.csv(input_file, check.names = FALSE)
dat <- unique(subset(raw, year == 2023))

dat$myc_type <- factor(toupper(dat$myc_type), levels = c("AM", "ECM"))
dat$mycorrhizal_legacy <- factor(
  toupper(dat$mycorrhizal_legacy), levels = c("AM", "ECM")
)
dat$group <- interaction(
  dat$myc_type, dat$mycorrhizal_legacy,
  sep = "_", drop = TRUE
)
dat$site_unit <- factor(dat$site_unit)
dat$species <- factor(toupper(dat$species))
dat$condition <- factor(dat$condition)

# Response-specific covariates match the existing manuscript models.
specifications <- list(
  height_change = list(
    label = "Height change (cm)",
    covariates = c(
      "leaf_percent_n",
      "leaf_percent_n:myc_type",
      "foliar_15n_enrichment",
      "foliar_15n_enrichment:myc_type"
    ),
    required = c("leaf_percent_n", "foliar_15n_enrichment"),
    random = c("condition", "site_unit", "species")
  ),
  foliar_15n_enrichment = list(
    label = "Foliar delta-15N enrichment (per mille)",
    covariates = "leaf_percent_n",
    required = "leaf_percent_n",
    random = c("site_unit", "species")
  ),
  leaf_percent_n = list(
    label = "Leaf N (%)",
    covariates = character(0),
    required = character(0),
    random = c("condition", "site_unit", "species")
  )
)

random_formula_text <- function(random_vars) {
  paste(sprintf("s(%s, bs = 're')", random_vars), collapse = " + ")
}

fixed_prefix <- function(spec) {
  terms <- c(spec$covariates, "group")
  paste(terms, collapse = " + ")
}

make_formulas <- function(response, spec) {
  prefix <- fixed_prefix(spec)
  random_text <- random_formula_text(spec$random)
  list(
    linear = as.formula(paste(
      response, "~", prefix, "+ dist_c:group +", random_text
    )),
    quadratic = as.formula(paste(
      response, "~", prefix,
      "+ dist_c:group + I(dist_c^2):group +", random_text
    )),
    gamm = as.formula(paste(
      response, "~", prefix,
      "+ s(dist_c, by = group, k = 4, bs = 'cr') +", random_text
    ))
  )
}

prepare_response_data <- function(response, spec) {
  needed <- unique(c(
    response, "distance_to_edge_m", "myc_type", "mycorrhizal_legacy",
    "group", spec$required, spec$random
  ))
  d <- dat[complete.cases(dat[, needed, drop = FALSE]), ]
  d$dist_center <- mean(d$distance_to_edge_m)
  d$dist_c <- d$distance_to_edge_m - d$dist_center
  d
}

fit_candidates <- function(response, spec, d, method = "ML") {
  forms <- make_formulas(response, spec)
  list(
    linear = gam(forms$linear, data = d, method = method),
    quadratic = gam(forms$quadratic, data = d, method = method),
    gamm = gam(forms$gamm, data = d, method = method, select = TRUE)
  )
}

population_exclusions <- function(spec) {
  sprintf("s(%s)", spec$random)
}

make_prediction_grid <- function(d, spec, length_out = 150) {
  groups <- levels(droplevels(d$group))
  rows <- lapply(groups, function(g) {
    z <- d[d$group == g, ]
    grid <- data.frame(
      distance_to_edge_m = seq(
        min(z$distance_to_edge_m), max(z$distance_to_edge_m), length.out = length_out
      )
    )
    grid$dist_center <- unique(d$dist_center)[1]
    grid$dist_c <- grid$distance_to_edge_m - grid$dist_center
    parts <- strsplit(g, "_", fixed = TRUE)[[1]]
    grid$myc_type <- factor(parts[1], levels = levels(d$myc_type))
    grid$mycorrhizal_legacy <- factor(parts[2], levels = levels(d$mycorrhizal_legacy))
    grid$group <- factor(g, levels = levels(d$group))

    # Hold continuous covariates at their overall means. Factors used as
    # random effects receive an existing reference level and are excluded
    # from population-level predictions below.
    for (v in spec$required) grid[[v]] <- mean(d[[v]], na.rm = TRUE)
    for (v in spec$random) {
      if (!v %in% names(grid)) grid[[v]] <- factor(levels(d[[v]])[1], levels = levels(d[[v]]))
    }
    grid
  })
  do.call(rbind, rows)
}

predict_candidates <- function(models, grid, spec) {
  exclusions <- population_exclusions(spec)
  do.call(rbind, lapply(names(models), function(model_name) {
    pr <- predict(
      models[[model_name]], newdata = grid, type = "link", se.fit = TRUE,
      exclude = exclusions
    )
    out <- grid
    out$model <- factor(
      model_name, levels = c("linear", "quadratic", "gamm"),
      labels = c("Linear", "Quadratic", "GAMM")
    )
    out$fit <- as.numeric(pr$fit)
    out$se <- as.numeric(pr$se.fit)
    out$lower <- out$fit - 1.96 * out$se
    out$upper <- out$fit + 1.96 * out$se
    out
  }))
}

model_comparison <- function(response, models, n) {
  aics <- AIC(models$linear, models$quadratic, models$gamm)
  out <- data.frame(
    response = response,
    model = c("linear", "quadratic", "gamm"),
    n = n,
    df = aics$df,
    AIC = aics$AIC,
    BIC = c(BIC(models$linear), BIC(models$quadratic), BIC(models$gamm)),
    deviance_explained = c(
      summary(models$linear)$dev.expl,
      summary(models$quadratic)$dev.expl,
      summary(models$gamm)$dev.expl
    )
  )
  out$delta_AIC <- out$AIC - min(out$AIC)
  out
}

linear_quadratic_lrt <- function(response, models) {
  test <- anova(models$linear, models$quadratic, test = "Chisq")
  test_df <- as.data.frame(test)
  p_column <- grep("^Pr\\(", names(test_df), value = TRUE)[1]
  df_difference <- attr(logLik(models$quadratic), "df") - attr(logLik(models$linear), "df")
  deviance_difference <- 2 * as.numeric(logLik(models$quadratic) - logLik(models$linear))
  data.frame(
    response = response,
    comparison = "linear_vs_quadratic",
    deviance_difference = deviance_difference,
    df_difference = df_difference,
    p_value = test_df[[p_column]][2]
  )
}

# Leave one site unit out. Predictions exclude all random-effect smooths, so
# scores measure population-level generalization to a held-out site.
site_blocked_cv <- function(response, spec, d) {
  sites <- levels(droplevels(d$site_unit))
  rows <- list()
  counter <- 1

  for (held_site in sites) {
    train <- d[d$site_unit != held_site, ]
    test <- d[d$site_unit == held_site, ]
    if (!nrow(test) || length(unique(train$distance_to_edge_m)) < 4) next

    models <- tryCatch(
      fit_candidates(response, spec, train, method = "ML"),
      error = function(e) NULL
    )
    if (is.null(models)) next

    exclusions <- population_exclusions(spec)
    for (model_name in names(models)) {
      pred <- tryCatch(
        # mgcv warns that the held-out site level was absent during fitting;
        # this is expected because site random effects are explicitly excluded.
        suppressWarnings(as.numeric(
          predict(models[[model_name]], newdata = test, exclude = exclusions)
        )),
        error = function(e) rep(NA_real_, nrow(test))
      )
      ok <- is.finite(pred) & is.finite(test[[response]])
      if (!any(ok)) next
      rows[[counter]] <- data.frame(
        response = response,
        held_out_site = held_site,
        model = model_name,
        n_test = sum(ok),
        SSE = sum((test[[response]][ok] - pred[ok])^2),
        SAE = sum(abs(test[[response]][ok] - pred[ok]))
      )
      counter <- counter + 1
    }
  }

  fold_results <- do.call(rbind, rows)
  aggregate_results <- do.call(rbind, lapply(split(fold_results, fold_results$model), function(z) {
    data.frame(
      response = response,
      model = z$model[1],
      n_predicted = sum(z$n_test),
      sites_scored = nrow(z),
      RMSE = sqrt(sum(z$SSE) / sum(z$n_test)),
      MAE = sum(z$SAE) / sum(z$n_test)
    )
  }))
  list(folds = fold_results, summary = aggregate_results)
}

make_comparison_plot <- function(response, spec, d, predictions) {
  ggplot(d, aes(distance_to_edge_m, .data[[response]])) +
    geom_point(aes(color = myc_type), alpha = 0.45, size = 1.7) +
    geom_ribbon(
      data = predictions[predictions$model == "GAMM", ],
      aes(x = distance_to_edge_m, ymin = lower, ymax = upper, group = group),
      inherit.aes = FALSE, fill = "grey55", alpha = 0.18
    ) +
    geom_line(
      data = predictions,
      aes(x = distance_to_edge_m, y = fit, color = myc_type, linetype = model),
      inherit.aes = FALSE, linewidth = 1
    ) +
    facet_grid(
      rows = vars(mycorrhizal_legacy), cols = vars(myc_type), scales = "free_y",
      labeller = labeller(
        myc_type = c(AM = "AM seedlings", ECM = "ECM seedlings"),
        mycorrhizal_legacy = c(AM = "AM legacy site", ECM = "ECM legacy site")
      )
    ) +
    scale_color_manual(values = c(AM = "#2166AC", ECM = "#D6604D")) +
    scale_linetype_manual(values = c(Linear = "solid", Quadratic = "dashed", GAMM = "dotdash")) +
    labs(
      x = "Distance from forest edge (m)", y = spec$label,
      color = "Mycorrhizal type", linetype = "Candidate model",
      title = paste(spec$label, "versus distance to edge"),
      subtitle = "Raw observations; population-level fits; grey ribbon is the GAMM 95% CI"
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "bottom", plot.title.position = "plot",
      strip.background = element_rect(fill = "grey95", color = "grey75"),
      strip.text = element_text(face = "bold")
    )
}

save_diagnostics <- function(response, models) {
  for (model_name in names(models)) {
    png(
      file.path(diagnostic_dir, paste0(response, "_", model_name, "_gam_check.png")),
      width = 1800, height = 1400, res = 180
    )
    gam.check(models[[model_name]])
    dev.off()
  }

  k <- as.data.frame(k.check(models$gamm))
  k$term <- rownames(k)
  rownames(k) <- NULL
  k$response <- response
  write.csv(k, file.path(diagnostic_dir, paste0(response, "_gamm_k_check.csv")), row.names = FALSE)
}

all_comparisons <- list()
all_lrt <- list()
all_cv_folds <- list()
all_cv_summary <- list()
all_smooth_tables <- list()
all_models <- list()

for (response in names(specifications)) {
  message("Analyzing ", response, "...")
  spec <- specifications[[response]]
  d <- prepare_response_data(response, spec)
  models <- fit_candidates(response, spec, d, method = "ML")
  all_models[[response]] <- models

  comparison <- model_comparison(response, models, nrow(d))
  lrt <- linear_quadratic_lrt(response, models)
  cv <- site_blocked_cv(response, spec, d)

  smooth_table <- as.data.frame(summary(models$gamm)$s.table)
  smooth_table$term <- rownames(smooth_table)
  rownames(smooth_table) <- NULL
  smooth_table$response <- response

  grid <- make_prediction_grid(d, spec)
  predictions <- predict_candidates(models, grid, spec)
  plot <- make_comparison_plot(response, spec, d, predictions)

  write.csv(
    predictions,
    file.path(output_dir, paste0(response, "_prediction_grid.csv")),
    row.names = FALSE
  )
  ggsave(
    file.path(figure_dir, paste0(response, "_linear_quadratic_gamm.png")),
    plot, width = 11, height = 8, dpi = 300, bg = "white"
  )
  save_diagnostics(response, models)

  all_comparisons[[response]] <- comparison
  all_lrt[[response]] <- lrt
  all_cv_folds[[response]] <- cv$folds
  all_cv_summary[[response]] <- cv$summary
  all_smooth_tables[[response]] <- smooth_table
}

write.csv(do.call(rbind, all_comparisons), file.path(output_dir, "model_information_criteria.csv"), row.names = FALSE)
write.csv(do.call(rbind, all_lrt), file.path(output_dir, "linear_vs_quadratic_lrt.csv"), row.names = FALSE)
write.csv(do.call(rbind, all_cv_folds), file.path(output_dir, "site_blocked_cv_folds.csv"), row.names = FALSE)
write.csv(do.call(rbind, all_cv_summary), file.path(output_dir, "site_blocked_cv_summary.csv"), row.names = FALSE)
write.csv(do.call(rbind, all_smooth_tables), file.path(output_dir, "gamm_smooth_tests.csv"), row.names = FALSE)

capture.output(
  lapply(all_models, function(x) lapply(x, summary)),
  file = file.path(output_dir, "model_summaries.txt")
)

cat("\nModel information criteria:\n")
print(do.call(rbind, all_comparisons))
cat("\nLinear versus quadratic likelihood-ratio tests:\n")
print(do.call(rbind, all_lrt))
cat("\nLeave-one-site-unit-out cross-validation:\n")
print(do.call(rbind, all_cv_summary))
