
# setup -------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)

# read in data

myc_alldata <-   
  read_csv("outplut/alldata_03262026.csv") %>% 
  mutate(
    site_unit = factor(site_unit)
  ) 


# sem hypothesized model --------------------------------------------------------------

#### hypothesized relationships ###

# seedling growth ~ leaf n concentration 
  # absolute growth to 2023, or could try just 2021 (why did we collect growth in 2023????)

# leaf n concentration ~ mycorrhizal mediation (15N enrichment) * mycorrhizal legacy * mycorrhizal association + proximity to root networks/competition (distance to edge)
  # myc mediation will depend on the mycorrhizal legacy and seedling association 

# mycorrhizal 15N ~ proximity to root networks (distance to edge ) * mycorrhizal legacy * mycorrhizal association
  # will depend on the mycorrhizal legacy and seedling association 

# SEM  --------------------------------------------------------------------
myc_2023 <- 
  myc_alldata %>%
  filter(year == "2023") %>%
  droplevels() %>%
  
  # turn the factors into numeric variables
  
  mutate(
    myc_legacy_num = case_when(mycorrhizal_legacy == "ecm" ~ 0, TRUE ~ 1),
    myc_type_num = case_when(myc_type == "ecm" ~ 0, TRUE ~ 1)
  )

# seedling model
# PC1 is postive height and rcd change, opposite of the seedling condition
mod_1 <-
  lme4::lmer(height_change ~  leaf_percent_n  * myc_legacy_num +  myc_type_num + distance_to_edge_m
           + (1 | site_unit) + (1 | species) ,
           data = myc_2023)
car::Anova(mod_1)
plot(resid(mod_1))

# percent N model 

mod_2 <-
  lme4::lmer(leaf_percent_n  ~  foliar_15n_enrichment + distance_to_edge_m
               + (1 | site_unit) + (1 | species) ,
             data = myc_2023)
summary(mod_2)
car::Anova(mod_2)
plot(resid(mod_2))

# percent N model 

mod_3 <-
  lme4::lmer(foliar_15n_enrichment  ~  distance_to_edge_m * myc_legacy_num * myc_type_num
               + (1 | site_unit) + (1 | species) ,
             data = myc_2023)
summary(mod_3)
car::Anova(mod_3)
plot(resid(mod_3))


# SEM

mod_global <- 
  piecewiseSEM::psem(mod_1, mod_2, mod_3)

summary(mod_global)
piecewiseSEM::dSep(mod_global)
piecewiseSEM::fisherC(mod_global)

plot(mod_global)

# Initial model work (remove when finalized -------------------------------



a <- lm(foliar_15N_enrichment ~ distance_to_edge_m+mycorrhizal_legacy + myc_type, 
        data = df)
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

