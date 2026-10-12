# Omnibus test of four mycorrhizal-type x legacy-specific quadratic curves.
# The full quadratic model is compared with the corresponding full linear
# model while preserving all original covariates and random intercepts.

library(tidyverse)
library(lme4)
library(lmerTest)
library(pbkrtest)

set.seed(20260813)
bootstrap_simulations <- 0 # set > 0 to request PBmodcomp simulations
output_dir <- "outplut/full_group_specific_quadratic_test"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dat <- read_csv("outplut/alldata_03262026.csv", show_col_types = FALSE) %>%
  distinct() %>%
  filter(year == "2023") %>%
  mutate(
    myc_type = factor(myc_type, levels = c("am", "ecm")),
    mycorrhizal_legacy = factor(mycorrhizal_legacy, levels = c("am", "ecm")),
    condition = factor(condition),
    site_unit = factor(site_unit),
    species = factor(species),
    distance_center = mean(distance_to_edge_m, na.rm = TRUE),
    dist_c = distance_to_edge_m - distance_center,
    dist_c2 = dist_c^2
  ) %>%
  droplevels()

specs <- list(
  height_change = list(
    covariates = c(
      "leaf_percent_n * myc_type",
      "foliar_15n_enrichment * myc_type"
    ),
    required = c("leaf_percent_n", "foliar_15n_enrichment"),
    random = c("(1 | condition)", "(1 | site_unit)", "(1 | species)"),
    random_vars = c("condition", "site_unit", "species")
  ),
  foliar_15n_enrichment = list(
    covariates = "leaf_percent_n",
    required = "leaf_percent_n",
    random = c("(1 | site_unit)", "(1 | species)"),
    random_vars = c("site_unit", "species")
  ),
  leaf_percent_n = list(
    covariates = character(0),
    required = character(0),
    random = c("(1 | condition)", "(1 | site_unit)", "(1 | species)"),
    random_vars = c("condition", "site_unit", "species")
  )
)

make_formula <- function(response, spec, quadratic = FALSE) {
  distance <- if (quadratic) {
    "(dist_c + dist_c2) * myc_type * mycorrhizal_legacy"
  } else {
    "dist_c * myc_type * mycorrhizal_legacy"
  }
  as.formula(paste(
    response, "~",
    paste(c(spec$covariates, distance, spec$random), collapse = " + ")
  ))
}

fit_ml <- function(formula, data) {
  lme4::lmer(
    formula, data = data, REML = FALSE,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

group_curvature <- function(model, response) {
  beta <- fixef(model)
  vc <- vcov(model)
  groups <- expand.grid(
    myc_type = c("am", "ecm"),
    mycorrhizal_legacy = c("am", "ecm"),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(groups)), function(i) {
    g <- groups[i, ]
    mf <- model.frame(model)
    nd <- mf[rep(1, 2), , drop = FALSE]
    nd$dist_c <- 0
    nd$dist_c2 <- c(0, 1)
    nd$myc_type <- factor(g$myc_type, levels = levels(mf$myc_type))
    nd$mycorrhizal_legacy <- factor(
      g$mycorrhizal_legacy, levels = levels(mf$mycorrhizal_legacy)
    )
    fixed_formula <- delete.response(terms(lme4::nobars(formula(model))))
    mm <- model.matrix(fixed_formula, nd)
    mm <- mm[, names(beta), drop = FALSE]
    contrast <- mm[2, ] - mm[1, ]
    estimate <- sum(contrast * beta)
    se <- sqrt(as.numeric(t(contrast) %*% vc %*% contrast))
    z <- estimate / se
    tibble(
      response = response,
      myc_type = g$myc_type,
      mycorrhizal_legacy = g$mycorrhizal_legacy,
      quadratic_estimate = estimate,
      std_error = se,
      z_value = z,
      p_value = 2 * pnorm(abs(z), lower.tail = FALSE),
      conf_low = estimate - 1.96 * se,
      conf_high = estimate + 1.96 * se
    )
  })
  bind_rows(rows)
}

comparisons <- list()
curvatures <- list()

for (response in names(specs)) {
  message("Testing ", response, "...")
  spec <- specs[[response]]
  needed <- unique(c(
    response, "distance_to_edge_m", "dist_c", "dist_c2",
    "myc_type", "mycorrhizal_legacy", spec$required, spec$random_vars
  ))
  d <- dat %>% filter(if_all(all_of(needed), ~ !is.na(.x))) %>% droplevels()

  linear <- fit_ml(make_formula(response, spec, FALSE), d)
  quadratic <- fit_ml(make_formula(response, spec, TRUE), d)
  # PBmodcomp retrieves the original data through the stored model call.
  # Point both calls at the response-specific data frame in this loop.
  linear@call$data <- quote(d)
  quadratic@call$data <- quote(d)
  linear@call$formula <- formula(linear)
  quadratic@call$formula <- formula(quadratic)
  lrt <- as.data.frame(anova(linear, quadratic, refit = FALSE))
  p_col <- grep("^Pr\\(", names(lrt), value = TRUE)[1]
  chisq_col <- grep("Chisq", names(lrt), value = TRUE)[1]

  if (bootstrap_simulations > 0) {
    pb <- PBmodcomp(quadratic, linear, nsim = bootstrap_simulations)
    pb_tests <- as.data.frame(pb$test)
    bootstrap_extremes <- pb$n.extreme
    bootstrap_p <- pb_tests["PBtest", "p.value"]
  } else {
    pb <- NULL
    bootstrap_extremes <- NA_integer_
    bootstrap_p <- NA_real_
  }

  comparisons[[response]] <- tibble(
    response = response,
    n = nrow(d),
    added_quadratic_df = attr(logLik(quadratic), "df") - attr(logLik(linear), "df"),
    likelihood_ratio_chisq = lrt[[chisq_col]][2],
    asymptotic_lrt_p = lrt[[p_col]][2],
    bootstrap_simulations_requested = bootstrap_simulations,
    bootstrap_extremes = bootstrap_extremes,
    bootstrap_p = bootstrap_p,
    linear_AIC = AIC(linear),
    quadratic_AIC = AIC(quadratic),
    delta_AIC_quadratic_minus_linear = AIC(quadratic) - AIC(linear),
    linear_BIC = BIC(linear),
    quadratic_BIC = BIC(quadratic),
    linear_singular = isSingular(linear),
    quadratic_singular = isSingular(quadratic)
  )
  curvatures[[response]] <- group_curvature(quadratic, response)

  capture.output(summary(linear), file = file.path(output_dir, paste0(response, "_full_linear.txt")))
  capture.output(summary(quadratic), file = file.path(output_dir, paste0(response, "_full_quadratic.txt")))
  if (!is.null(pb)) {
    capture.output(pb, file = file.path(output_dir, paste0(response, "_bootstrap_test.txt")))
  }
}

comparison_table <- bind_rows(comparisons)
curvature_table <- bind_rows(curvatures)
write_csv(comparison_table, file.path(output_dir, "full_quadratic_vs_full_linear.csv"))
write_csv(curvature_table, file.path(output_dir, "group_specific_quadratic_contrasts.csv"))

cat("\nFull quadratic block tests:\n")
print(comparison_table)
cat("\nGroup-specific curvature contrasts (descriptive unless omnibus test is supported):\n")
print(curvature_table)
