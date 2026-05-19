

# setup -------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)


# read in data

myc_alldata <-   
  read_csv("outplut/alldata_03262026.csv") %>% 
  mutate(
    site_unit = factor(site_unit)
  ) 

myc_2023 <- 
  myc_alldata %>%
  filter(year == "2023") %>%
  droplevels() %>%
  
  # turn the factors into numeric variables
  
  mutate(
    myc_legacy_num = case_when(mycorrhizal_legacy == "ecm" ~ 0, TRUE ~ 1),
    myc_type_num = case_when(myc_type == "ecm" ~ 0, TRUE ~ 1),
    
    # scale numeric variables
    
    dist_s = scale(distance_to_edge_m)[,1]
  )

# Plotting theme ---------------------------------------------------------

my_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = element_text(size = 8),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      strip.text.x = element_text(size = 8),
      legend.position = "bottom"
    )
}

# Height change model and figure  ------------------------------------------------------------

# height change

mod_2 <-
  lme4::lmer(height_change ~  
               leaf_percent_n  * myc_type_num + 
               foliar_15n_enrichment * myc_type_num + 
               distance_to_edge_m * myc_type_num * myc_legacy_num +
               #herbivory +
               (1 | condition ) + 
               (1 | site_unit)  + (1 | species) ,
             data = myc_2023)

AIC(mod_alt, mod_2)
summary(mod_2)
car::Anova(mod_2)
plot(resid(mod_2)~ myc_2023$leaf_percent_n)

emmeans::emtrends(mod_2 ,~ myc_type_num * myc_legacy_num * leaf_percent_n, var = "leaf_percent_n")
emmeans::emtrends(mod_2 , ~ leaf_percent_n*myc_type_num, var = "leaf_percent_n")
emmeans::emtrends(mod_2 , ~ distance_to_edge_m*myc_type_num, var = "distance_to_edge_m")

# create predicted dataset

predicted_data <- 
  
  expand_grid(
  distance_to_edge_m = seq(
    min(myc_2023$distance_to_edge_m, na.rm = TRUE),
    max(myc_2023$distance_to_edge_m, na.rm = TRUE),
    length.out = 100
  ),
  myc_type_num = unique(myc_2023$myc_type_num)
) %>%
  mutate(
    myc_legacy_num = 0,
    leaf_percent_n = mean(myc_2023$leaf_percent_n, na.rm = TRUE),
    foliar_15n_enrichment = mean(myc_2023$foliar_15n_enrichment, na.rm = TRUE),
    
    condition = NA,
    site_unit = NA,
    species = NA
  ) %>%
  mutate(
    pred_height = predict(
      mod_2,
      newdata = pick(everything()),
      re.form = NA
    )
  )

X_pred <- model.matrix(
  ~ leaf_percent_n * myc_type_num +
    foliar_15n_enrichment * myc_type_num +
    distance_to_edge_m * myc_type_num * myc_legacy_num,
  data = predicted_data
)

se_preds <- sqrt(
  diag(X_pred %*% vcov(mod_2) %*% t(X_pred))
)

predicted_data <- predicted_data %>%
  mutate(
    se = se_preds,
    lower = pred_height - 1.96 * se,
    upper = pred_height + 1.96 * se
  )

# plot
height_distedge <-

ggplot(
  predicted_data,
  aes(
    x = distance_to_edge_m,
    y = pred_height,
    color = factor(myc_type_num),
    fill = factor(myc_type_num)
  )
) +
  geom_line(linewidth = 2) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2,
    color = NA
  ) + 
  labs(
    y = expression("Predicted seedling height (cm)",
    x = "Distance from plot edge (m)") ) +
  annotate("text", label = "B", y= max(predicted_data$upper - 1), 
           x = min(myc_2023$distance_to_edge_m ), 
           hjust=1, size = 4) +
  my_theme()
