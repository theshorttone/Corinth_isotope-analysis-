
# setup -------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
df <- realmerTesta <- read_csv("outplut/combined_soil_leaf_with_myc_type") %>% 
  mutate(
    site_unit = factor(site_unit)
  )

# SEM code ----------------------------------------------------------------



a <- lm(leaf_d15n ~ distance_to_edge_m+mycorrhizal_legacy+myc_type, data = df)
summary(a)
AIC(a)

b <- glm(leaf_d15n ~ distance_to_edge_m*myc_type+mycorrhizal_legacy, data = df)
summary(b)

#
c <- lmer(leaf_d15n ~ distance_to_edge_m+mycorrhizal_legacy+myc_type+(1 | site_unit), data = df)
summary(c)
AIC(c)

#best model is the one with species in it, as species explains more variation that just mycorrhizal type.
d <- lmer(leaf_d15n ~ distance_to_edge_m+species+(1 | site_unit), data = df)
summary(d)
anova(d, type = 3)
AIC(d)


check_model(c)
check_model(d)
#looks awesome 

