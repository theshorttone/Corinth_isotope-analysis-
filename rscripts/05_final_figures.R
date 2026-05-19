

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

# Height change ~ distance to edge  ------------------------------------------------------------

# height change

mod_2 <-
  lme4::lmer(height_change ~  
               leaf_percent_n  * myc_type + 
               foliar_15n_enrichment * myc_type + 
               distance_to_edge_m * myc_type * mycorrhizal_legacy +
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

# predicted values for plotting CIs and trend lines 

predicted_data <- 
  
  tibble(
  distance_to_edge_m = rep(
    unique(myc_2023$distance_to_edge_m),
    times = length(unique(myc_2023$myc_type))
  ),
  
  myc_type = rep(
    unique(myc_2023$myc_type),
    each = length(unique(myc_2023$distance_to_edge_m))
  ),
  
  leaf_percent_n = mean(myc_2023$leaf_percent_n, na.rm = TRUE),
  
  foliar_15n_enrichment = mean(
    myc_2023$foliar_15n_enrichment,
    na.rm = TRUE
  ),
  
  condition = NA,
  site_unit = NA,
  species = NA
) |>
  
  dplyr::mutate(
    mycorrhizal_legacy = factor(
      "am",
      levels = levels(factor(myc_2023$mycorrhizal_legacy))
    ),
    
    pred_height = predict(
      mod_2,
      newdata = dplyr::cur_data(),
      re.form = NA
    )
  )

X_pred <- model.matrix(
  ~  leaf_percent_n * myc_type +
    foliar_15n_enrichment * myc_type +
    distance_to_edge_m * myc_type * mycorrhizal_legacy,
  data = predicted_data
)

se_preds <- sqrt(
  diag(X_pred %*% vcov(mod_2) %*% t(X_pred))
)

predicted_data <- 
  predicted_data %>%
  mutate(
    se = se_preds,
    lower = pred_height - 1.96 * se,
    upper = pred_height + 1.96 * se
  )

# plot
height_distedge <-

ggplot() +
geom_point(data = myc_2023,
           aes(x = distance_to_edge_m,
               y = height_change,
           color = myc_type,
           fill = myc_type ) ) +
  geom_line(data = predicted_data,
            aes(
              x = distance_to_edge_m,
              y = pred_height,
              color = factor(myc_type)),
              linewidth = 2 ) +
  geom_ribbon(data = predicted_data,
        aes(
          x = distance_to_edge_m,
          y = pred_height,
          ymin = lower, 
          ymax = upper,
          color = factor(myc_type),
          fill = factor(myc_type)),
    alpha = 0.2
  ) + 
  labs(
    y = expression("Predicted seedling height (cm)",
    x = "Distance from plot edge (m)") ) +
  annotate("text", label = "B", y= max(predicted_data$upper - 1), 
           x = min(myc_2023$distance_to_edge_m ), 
           hjust=1, size = 4) +
  my_theme()
