# Linear versus quadratic distance models: collaborator review
# =============================================================
# This script is deliberately repetitive. Each biological response is analyzed
# separately with the same fixed- and random-effect structure.
#
# For each response we fit:
#   1. A linear distance model for all four myc type x site legacy groups.
#   2. The linear model plus one overall quadratic term shared by every group.
#   3. The linear model plus quadratic curvature that differs by myc type.
#   4. A quadratic model for all four myc type x site legacy groups.
#
# The three-way interaction in the linear model allows the distance slope to
# differ among the four myc type x legacy combinations. We then add curvature
# gradually: overall, by myc type, and finally by myc type x site legacy.
#
# The response variables are NOT used as covariates for one another. Height,
# foliar delta-15N enrichment, and leaf N are three independent analyses.

library(tidyverse)
library(lme4)
library(lmerTest)

output_dir <- "outplut/collaborator_review_linear_vs_quadratic"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# The source file contains each 2023 observation twice. distinct() removes the
# exact duplicates so sample size, standard errors, and p-values are correct.
dat <- read_csv("outplut/alldata_03262026.csv", show_col_types = FALSE) %>%
  distinct() %>%
  filter(year == "2023") %>%
  mutate(
    myc_type = factor(myc_type, levels = c("am", "ecm")),
    mycorrhizal_legacy = factor(mycorrhizal_legacy, levels = c("am", "ecm")),
    condition = factor(condition),
    site_unit = factor(site_unit),
    species = factor(species),
    # Centering distance reduces correlation between x and x squared.
    # dist_c = 0 is the mean observed distance from the edge.
    distance_center = mean(distance_to_edge_m, na.rm = TRUE),
    dist_c = distance_to_edge_m - distance_center,
    dist_c2 = dist_c^2
  ) %>%
  droplevels()

fit_control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000)
)

# We use maximum likelihood (REML = FALSE) because the linear and quadratic
# models have different fixed effects and will be compared with an LRT and AIC.


# -----------------------------------------------------------------------------
# 1. HEIGHT CHANGE
# -----------------------------------------------------------------------------

height_dat <- dat %>%
  filter(
    !is.na(height_change),
    !is.na(dist_c),
    !is.na(myc_type),
    !is.na(mycorrhizal_legacy),
    !is.na(condition),
    !is.na(site_unit),
    !is.na(species)
  ) %>%
  droplevels()

# LINEAR MODEL
# dist_c * myc_type * mycorrhizal_legacy expands to all main effects,
# all two-way interactions, and the three-way interaction.
height_linear <- lmer(
  height_change ~
    dist_c * myc_type * mycorrhizal_legacy +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = height_dat, REML = FALSE, control = fit_control
)

# OVERALL QUADRATIC MODEL
# Add one dist_c2 coefficient shared by all four groups.
height_overall_quadratic <- lmer(
  height_change ~
    dist_c * myc_type * mycorrhizal_legacy + dist_c2 +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = height_dat, REML = FALSE, control = fit_control
)

# MYC-SPECIFIC QUADRATIC MODEL
# dist_c2 * myc_type gives AM and EcM different curvature, while legacy is not
# allowed to alter curvature. Linear slopes can still differ by myc x legacy.
height_myc_quadratic <- lmer(
  height_change ~
    dist_c * myc_type * mycorrhizal_legacy + dist_c2 * myc_type +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = height_dat, REML = FALSE, control = fit_control
)

# FULL GROUP-SPECIFIC QUADRATIC MODEL
# This allows curvature to differ among all four myc type x legacy groups.
height_group_quadratic <- lmer(
  height_change ~
    (dist_c + dist_c2) * myc_type * mycorrhizal_legacy +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = height_dat, REML = FALSE, control = fit_control
)

# Coefficient tables give the estimate, SE, test statistic, and p-value for each
# individual model term. Interaction coefficients are differences from the
# reference group (AM seedlings in AM legacy sites), not four standalone curves.
cat("\nHEIGHT: linear-model coefficient tests\n")
print(coef(summary(height_linear)))
cat("\nHEIGHT: overall-quadratic coefficient tests\n")
print(coef(summary(height_overall_quadratic)))
cat("\nHEIGHT: myc-specific-quadratic coefficient tests\n")
print(coef(summary(height_myc_quadratic)))
cat("\nHEIGHT: full group-specific-quadratic coefficient tests\n")
print(coef(summary(height_group_quadratic)))

# The AIC table compares overall support. Lower AIC is better; a difference
# smaller than about 2 provides little separation between the models.
height_AIC <- AIC(
  height_linear, height_overall_quadratic,
  height_myc_quadratic, height_group_quadratic
)
rownames(height_AIC) <- c(
  "linear", "overall_quadratic", "myc_specific_quadratic",
  "myc_x_legacy_specific_quadratic"
)
cat("\nHEIGHT: AIC for all four candidate models\n")
print(height_AIC)

# Nested tests add curvature one step at a time.
height_test_overall_quadratic <- anova(
  height_linear, height_overall_quadratic, refit = FALSE
)
height_test_curvature_by_myc <- anova(
  height_overall_quadratic, height_myc_quadratic, refit = FALSE
)
height_test_curvature_by_legacy <- anova(
  height_myc_quadratic, height_group_quadratic, refit = FALSE
)
height_test_all_quadratic_terms <- anova(
  height_linear, height_group_quadratic, refit = FALSE
)
height_quadratic_tests <- list(
  "1: Does one overall quadratic term improve the linear model?" = height_test_overall_quadratic,
  "2: Does curvature differ by myc type?" = height_test_curvature_by_myc,
  "3: Does curvature additionally differ by legacy?" = height_test_curvature_by_legacy,
  "4: Do all four curvature terms jointly improve the linear model?" = height_test_all_quadratic_terms
)
cat("\nHEIGHT: nested likelihood-ratio tests\n")
print(height_quadratic_tests)


# -----------------------------------------------------------------------------
# 2. FOLIAR DELTA-15N ENRICHMENT
# -----------------------------------------------------------------------------

n15_dat <- dat %>%
  filter(
    !is.na(foliar_15n_enrichment),
    !is.na(dist_c),
    !is.na(myc_type),
    !is.na(mycorrhizal_legacy),
    !is.na(condition),
    !is.na(site_unit),
    !is.na(species)
  ) %>%
  droplevels()

n15_linear <- lmer(
  foliar_15n_enrichment ~
    dist_c * myc_type * mycorrhizal_legacy +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = n15_dat, REML = FALSE, control = fit_control
)

n15_overall_quadratic <- lmer(
  foliar_15n_enrichment ~
    dist_c * myc_type * mycorrhizal_legacy + dist_c2 +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = n15_dat, REML = FALSE, control = fit_control
)

n15_myc_quadratic <- lmer(
  foliar_15n_enrichment ~
    dist_c * myc_type * mycorrhizal_legacy + dist_c2 * myc_type +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = n15_dat, REML = FALSE, control = fit_control
)

n15_group_quadratic <- lmer(
  foliar_15n_enrichment ~
    (dist_c + dist_c2) * myc_type * mycorrhizal_legacy +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = n15_dat, REML = FALSE, control = fit_control
)

cat("\nFOLIAR DELTA-15N: linear-model coefficient tests\n")
print(coef(summary(n15_linear)))
cat("\nFOLIAR DELTA-15N: overall-quadratic coefficient tests\n")
print(coef(summary(n15_overall_quadratic)))
cat("\nFOLIAR DELTA-15N: myc-specific-quadratic coefficient tests\n")
print(coef(summary(n15_myc_quadratic)))
cat("\nFOLIAR DELTA-15N: full group-specific-quadratic coefficient tests\n")
print(coef(summary(n15_group_quadratic)))

n15_AIC <- AIC(
  n15_linear, n15_overall_quadratic,
  n15_myc_quadratic, n15_group_quadratic
)
rownames(n15_AIC) <- c(
  "linear", "overall_quadratic", "myc_specific_quadratic",
  "myc_x_legacy_specific_quadratic"
)
cat("\nFOLIAR DELTA-15N: AIC for all four candidate models\n")
print(n15_AIC)

n15_test_overall_quadratic <- anova(
  n15_linear, n15_overall_quadratic, refit = FALSE
)
n15_test_curvature_by_myc <- anova(
  n15_overall_quadratic, n15_myc_quadratic, refit = FALSE
)
n15_test_curvature_by_legacy <- anova(
  n15_myc_quadratic, n15_group_quadratic, refit = FALSE
)
n15_test_all_quadratic_terms <- anova(
  n15_linear, n15_group_quadratic, refit = FALSE
)
n15_quadratic_tests <- list(
  "1: Does one overall quadratic term improve the linear model?" = n15_test_overall_quadratic,
  "2: Does curvature differ by myc type?" = n15_test_curvature_by_myc,
  "3: Does curvature additionally differ by legacy?" = n15_test_curvature_by_legacy,
  "4: Do all four curvature terms jointly improve the linear model?" = n15_test_all_quadratic_terms
)
cat("\nFOLIAR DELTA-15N: nested likelihood-ratio tests\n")
print(n15_quadratic_tests)


# -----------------------------------------------------------------------------
# 3. LEAF N (%)
# -----------------------------------------------------------------------------

leaf_n_dat <- dat %>%
  filter(
    !is.na(leaf_percent_n),
    !is.na(dist_c),
    !is.na(myc_type),
    !is.na(mycorrhizal_legacy),
    !is.na(condition),
    !is.na(site_unit),
    !is.na(species)
  ) %>%
  droplevels()

leaf_n_linear <- lmer(
  leaf_percent_n ~
    dist_c * myc_type * mycorrhizal_legacy +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = leaf_n_dat, REML = FALSE, control = fit_control
)

leaf_n_overall_quadratic <- lmer(
  leaf_percent_n ~
    dist_c * myc_type * mycorrhizal_legacy + dist_c2 +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = leaf_n_dat, REML = FALSE, control = fit_control
)

leaf_n_myc_quadratic <- lmer(
  leaf_percent_n ~
    dist_c * myc_type * mycorrhizal_legacy + dist_c2 * myc_type +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = leaf_n_dat, REML = FALSE, control = fit_control
)

leaf_n_group_quadratic <- lmer(
  leaf_percent_n ~
    (dist_c + dist_c2) * myc_type * mycorrhizal_legacy +
    (1 | condition) + (1 | site_unit) + (1 | species),
  data = leaf_n_dat, REML = FALSE, control = fit_control
)

cat("\nLEAF N: linear-model coefficient tests\n")
print(coef(summary(leaf_n_linear)))
cat("\nLEAF N: overall-quadratic coefficient tests\n")
print(coef(summary(leaf_n_overall_quadratic)))
cat("\nLEAF N: myc-specific-quadratic coefficient tests\n")
print(coef(summary(leaf_n_myc_quadratic)))
cat("\nLEAF N: full group-specific-quadratic coefficient tests\n")
print(coef(summary(leaf_n_group_quadratic)))

leaf_n_AIC <- AIC(
  leaf_n_linear, leaf_n_overall_quadratic,
  leaf_n_myc_quadratic, leaf_n_group_quadratic
)
rownames(leaf_n_AIC) <- c(
  "linear", "overall_quadratic", "myc_specific_quadratic",
  "myc_x_legacy_specific_quadratic"
)
cat("\nLEAF N: AIC for all four candidate models\n")
print(leaf_n_AIC)

leaf_n_test_overall_quadratic <- anova(
  leaf_n_linear, leaf_n_overall_quadratic, refit = FALSE
)
leaf_n_test_curvature_by_myc <- anova(
  leaf_n_overall_quadratic, leaf_n_myc_quadratic, refit = FALSE
)
leaf_n_test_curvature_by_legacy <- anova(
  leaf_n_myc_quadratic, leaf_n_group_quadratic, refit = FALSE
)
leaf_n_test_all_quadratic_terms <- anova(
  leaf_n_linear, leaf_n_group_quadratic, refit = FALSE
)
leaf_n_quadratic_tests <- list(
  "1: Does one overall quadratic term improve the linear model?" = leaf_n_test_overall_quadratic,
  "2: Does curvature differ by myc type?" = leaf_n_test_curvature_by_myc,
  "3: Does curvature additionally differ by legacy?" = leaf_n_test_curvature_by_legacy,
  "4: Do all four curvature terms jointly improve the linear model?" = leaf_n_test_all_quadratic_terms
)
cat("\nLEAF N: nested likelihood-ratio tests\n")
print(leaf_n_quadratic_tests)


# -----------------------------------------------------------------------------
# SAVE RESULTS
# -----------------------------------------------------------------------------

write.csv(height_AIC, file.path(output_dir, "height_linear_vs_quadratic_AIC.csv"))
write.csv(height_test_overall_quadratic, file.path(output_dir, "height_test_1_overall_quadratic.csv"))
write.csv(height_test_curvature_by_myc, file.path(output_dir, "height_test_2_curvature_by_myc.csv"))
write.csv(height_test_curvature_by_legacy, file.path(output_dir, "height_test_3_curvature_by_legacy.csv"))
write.csv(height_test_all_quadratic_terms, file.path(output_dir, "height_test_4_all_quadratic_terms.csv"))
write.csv(coef(summary(height_linear)), file.path(output_dir, "height_linear_coefficients.csv"))
write.csv(coef(summary(height_overall_quadratic)), file.path(output_dir, "height_overall_quadratic_coefficients.csv"))
write.csv(coef(summary(height_myc_quadratic)), file.path(output_dir, "height_myc_quadratic_coefficients.csv"))
write.csv(coef(summary(height_group_quadratic)), file.path(output_dir, "height_group_quadratic_coefficients.csv"))

write.csv(n15_AIC, file.path(output_dir, "n15_linear_vs_quadratic_AIC.csv"))
write.csv(n15_test_overall_quadratic, file.path(output_dir, "n15_test_1_overall_quadratic.csv"))
write.csv(n15_test_curvature_by_myc, file.path(output_dir, "n15_test_2_curvature_by_myc.csv"))
write.csv(n15_test_curvature_by_legacy, file.path(output_dir, "n15_test_3_curvature_by_legacy.csv"))
write.csv(n15_test_all_quadratic_terms, file.path(output_dir, "n15_test_4_all_quadratic_terms.csv"))
write.csv(coef(summary(n15_linear)), file.path(output_dir, "n15_linear_coefficients.csv"))
write.csv(coef(summary(n15_overall_quadratic)), file.path(output_dir, "n15_overall_quadratic_coefficients.csv"))
write.csv(coef(summary(n15_myc_quadratic)), file.path(output_dir, "n15_myc_quadratic_coefficients.csv"))
write.csv(coef(summary(n15_group_quadratic)), file.path(output_dir, "n15_group_quadratic_coefficients.csv"))

write.csv(leaf_n_AIC, file.path(output_dir, "leaf_n_linear_vs_quadratic_AIC.csv"))
write.csv(leaf_n_test_overall_quadratic, file.path(output_dir, "leaf_n_test_1_overall_quadratic.csv"))
write.csv(leaf_n_test_curvature_by_myc, file.path(output_dir, "leaf_n_test_2_curvature_by_myc.csv"))
write.csv(leaf_n_test_curvature_by_legacy, file.path(output_dir, "leaf_n_test_3_curvature_by_legacy.csv"))
write.csv(leaf_n_test_all_quadratic_terms, file.path(output_dir, "leaf_n_test_4_all_quadratic_terms.csv"))
write.csv(coef(summary(leaf_n_linear)), file.path(output_dir, "leaf_n_linear_coefficients.csv"))
write.csv(coef(summary(leaf_n_overall_quadratic)), file.path(output_dir, "leaf_n_overall_quadratic_coefficients.csv"))
write.csv(coef(summary(leaf_n_myc_quadratic)), file.path(output_dir, "leaf_n_myc_quadratic_coefficients.csv"))
write.csv(coef(summary(leaf_n_group_quadratic)), file.path(output_dir, "leaf_n_group_quadratic_coefficients.csv"))

# Full summaries include fixed effects, random effects, residual information,
# and convergence details.
capture.output(summary(height_linear), file = file.path(output_dir, "height_linear_full_summary.txt"))
capture.output(summary(height_overall_quadratic), file = file.path(output_dir, "height_overall_quadratic_full_summary.txt"))
capture.output(summary(height_myc_quadratic), file = file.path(output_dir, "height_myc_quadratic_full_summary.txt"))
capture.output(summary(height_group_quadratic), file = file.path(output_dir, "height_group_quadratic_full_summary.txt"))
capture.output(summary(n15_linear), file = file.path(output_dir, "n15_linear_full_summary.txt"))
capture.output(summary(n15_overall_quadratic), file = file.path(output_dir, "n15_overall_quadratic_full_summary.txt"))
capture.output(summary(n15_myc_quadratic), file = file.path(output_dir, "n15_myc_quadratic_full_summary.txt"))
capture.output(summary(n15_group_quadratic), file = file.path(output_dir, "n15_group_quadratic_full_summary.txt"))
capture.output(summary(leaf_n_linear), file = file.path(output_dir, "leaf_n_linear_full_summary.txt"))
capture.output(summary(leaf_n_overall_quadratic), file = file.path(output_dir, "leaf_n_overall_quadratic_full_summary.txt"))
capture.output(summary(leaf_n_myc_quadratic), file = file.path(output_dir, "leaf_n_myc_quadratic_full_summary.txt"))
capture.output(summary(leaf_n_group_quadratic), file = file.path(output_dir, "leaf_n_group_quadratic_full_summary.txt"))
