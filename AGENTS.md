# Corinth isotope analysis

Shared R project (Zac Shortt and Amelia Fitch) on how distance to the forest
edge, tree mycorrhizal type (AM vs. EcM), and site mycorrhizal legacy affect
seedling leaf N, height growth, and foliar δ15N enrichment at the Corinth
harvest plots.

## Working rules

- **This is a shared repo.** Before editing scripts, `git fetch` and check
  `origin/main` for collaborator commits; don't overwrite their changes.
- Run scripts from the project root (paths are relative to it, e.g.
  `Rscript rscripts/05_final_figures.R`).
- Outputs go in `outplut/` (sic, the typo is the real folder name). `outputs/`
  is a separate, unused folder.
- Style: tidyverse pipes, `lme4::lmer` models, `car::Anova(type = 2)`,
  `emmeans`/`emtrends` for marginal means and slopes.
- Supplemental tables are Word files built with `flextable` (theme_vanilla,
  bold header, centered, Times New Roman 12 pt, model name shown only on its
  first row), saved to `outplut/tables/` with a hard-coded MMDDYYYY date in the
  file name.

## Pipeline

1. `rscripts/01_leaf_soil_combination_data cleaning.R`: cleans leaf/soil
   isotope lab sheets, drops failed QA/QC soil samples, averages duplicates,
   joins distance-to-edge key → `outplut/combined_soil_leaf`.
2. `rscripts/02_data_distributions.r`: assigns site legacy (by `site_unit`)
   and tree myc type (by species); computes foliar 15N enrichment
   (leaf δ15N − soil δ15N, Cheng et al. 2010)
   → `outplut/combined_soil_leaf_with_myc_type_and_enrichment.csv`.
3. `rscripts/04_surival analysis.r` (Amelia): seedling demography, height/RCD
   change, biomass growth; joins isotope data, drops TIAM, `na.omit()`
   → **`outplut/alldata_03262026.csv`**, the input for all later scripts.
4. `rscripts/03_structural equasions model.r`: piecewiseSEM.
5. `rscripts/05_final_figures.R`: **manuscript script.** Figure 2 (leaf N,
   height change, foliar 15N vs. distance; `lmer` with
   distance × myc_type × mycorrhizal_legacy + condition + (1|site_unit) +
   (1|species)), Figure S1 (soil N and C:N by legacy), and the supplemental
   Word tables.
6. `rscripts/06_supplemental_analyses_figures.R` (Amelia): pre-harvest soil
   C:N by legacy from `data/Corinth.alldata.csv`.

Exploratory / archived:
- `05_final_figures_quadratic.R`, `06_review_linear_vs_quadratic.R`,
  `06_test_quadratic_fits.R`, `07`–`13`: linear vs. quadratic vs. GAMM
  distance-response comparisons. The final models are linear.
- `outtakes.R`: analyses removed from 05 when the figure set was narrowed.
- `decoupling_sourcescript (3).R`: unrelated HJ Andrews microclimate script.

## Known data issues

- `alldata_03262026.csv` contains every 2023 row twice (342 rows, 171 unique).
  Always `distinct()` after filtering to 2023; the root cause is likely a
  non-unique join key in script 04.
- Script 01 references `df_leaf_clean` before it is defined, so it won't run
  top to bottom as is.
- `condition` is 0/1 (set in script 04); it's a fixed effect in 05 but a random
  intercept in 03 and 11–13.
