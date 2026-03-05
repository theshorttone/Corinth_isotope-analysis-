
# setup -------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
df <- realmerTesta <- read_csv("outplut/combined_soil_leaf_with_myc_type_and_enrichment.csv") %>% 
  mutate(
    site_unit = factor(site_unit)
  ) %>% 
  filter(!(species == "TIAM"))


# models ------------------------------------------------------------------


a <- lm(foliar_15N_enrichment ~ distance_to_edge_m+mycorrhizal_legacy+myc_type, data = df)
summary(a)
AIC(a)

b <- glm(foliar_15N_enrichment ~ distance_to_edge_m*myc_type+mycorrhizal_legacy, data = df)
summary(b)

b <- glm(foliar_15N_enrichment ~ distance_to_edge_m*myc_type+mycorrhizal_legacy*myc_type, data = df)
summary(b)

#
c <- lmer(foliar_15N_enrichment ~ distance_to_edge_m+mycorrhizal_legacy+myc_type+(1 | site_unit), data = df)
summary(c)
AIC(c)

#best model is the one with species in it, as species explains more variation that just mycorrhizal type.
d <- lmer(foliar_15N_enrichment ~ distance_to_edge_m + species + (1 | site_unit), data = df)
summary(d)
anova(d, type = 3)
AIC(d)

e <- lmer(foliar_15N_enrichment ~ distance_to_edge_m * myc_type + mycorrhizal_legacy + (1 | site_unit)+ 
            (1 | species), data = df)
summary(e)
AIC(e)

e_nested <- lmer(
  leaf_d15n ~ distance_to_edge_m + myc_type +
    (1 | site_unit) +
    (1 + distance_to_edge_m | species),
  data = df, REML = FALSE
)

e_nested
summary(e_nested)

check_model(c)
check_model(d)
#looks awesome 

