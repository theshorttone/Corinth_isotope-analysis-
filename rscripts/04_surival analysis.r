# Amelia Fitch
# seedling survival analyses

# setup  ----------------------------------------------------------------

library(tidyverse)
# install.packages("brms")
# install.packages("mgcv")


# Function to clean data 


clean_colnames <- function(df) {
  df %>%
    # clean column names
    rename_with(~ .x %>%
                  str_replace_all("`", "") %>%
                  str_replace_all("\\s+", "_") %>%
                  str_remove_all("\\(.*?\\)") %>%
                  str_replace_all("_+", "_") %>%
                  str_replace_all("_$", "") %>%
                  str_replace_all("\\.", "_") %>%
                  tolower()
    ) %>%
    # remove NA columns
    select(
      where(~ !all(is.na(.x))) ) %>%
    filter(
      !if_all(everything(), is.na) ) %>%
    # convert ALL non-numeric columns to lowercase character
    mutate(
      across(
        .cols = where(~ !is.numeric(.x)), 
        .fns = ~ .x %>%
          as.character() %>%         # converts factors, logicals, etc.
          tolower() %>%
          str_replace_all("\\s+", "_")
      )
    )
}

# Plotting theme 

my_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      strip.text.x = element_text(size = 12),
      legend.position = "bottom"
    )
}

# colors for graphing

blues   <- RColorBrewer::brewer.pal(9, "Blues")
oranges <- RColorBrewer::brewer.pal(9, "Oranges")

species_palette <- c(
  "prse" = blues[3],
  "acsa" = blues[5],
  "acru" = blues[7],
  "nysy" = blues[9],
  "bele" = oranges[3],
  "quru" = oranges[5],
  "tiam" = oranges[7],
  "caco" = oranges[9]
)
# data processing -------------------------------------------------------------

# Sampling date and time for growth calculations

planting_dates <-
  
  read_csv("data/Corinth_seedling_data.csv") %>%
  clean_colnames() %>%
  # filter(year == "2021") %>%
  select(unit, plot, planting_date, time_months, year) %>%
  distinct() %>% 
  drop_na() %>%
  
  mutate(
    planting_date = as.Date(planting_date, format = "%m/%d/%y")
  ) %>%
  
  { bind_rows(
    .,
    mutate(
      .,
      time_months = as.numeric(
        difftime(as.Date("2023-09-02"), planting_date, units = "days")
      ) / 30.44,
      year = 2023
    )
  )
  } %>%
  distinct()

# full seedling dataset

seedling_data <- 
  # 2021 - 2022 seedling survival and growth data
  read_csv("data/seedling_demography/ Corinth_seedling_data.csv") %>%
  clean_colnames() %>%
  select(year, unit, plot, species, number, ht_cm_initial, rcd_mm_initial, ht_cm, rcd_mm, herbivory, condition, health) %>%
  
  # add the 2023 data
  
  bind_rows(
    read_csv("data/seedling_demography/seedling_2023.csv") %>%
      clean_colnames() %>%
      mutate(year = 2023,
             ht_cm = as.numeric(ht_cm),
             rcd_mm = as.numeric(rcd_mm),
             herbivory = as.numeric(herbivory),
             condition = case_when(condition == "true" ~ 1,
                                   TRUE ~ 0)) %>%
      
      # add the initial height column
      
      left_join(
        read_csv("data/seedling_demography/ Corinth_seedling_data.csv") %>%
          clean_colnames() %>%
          select(unit, plot, species, number, ht_cm_initial, rcd_mm_initial), 
        by = c("unit", "plot", "species", "number")
        )
  ) %>%
  mutate(
    status = case_when(health == "d" ~ 0, 
                       TRUE ~ 1), 
    
    height_change = ht_cm - ht_cm_initial,
    rcd_change = rcd_mm - rcd_mm_initial ) %>%
  
  # pull 2022 positive growth values
  # left_join(
  #   seedling_data %>%
  #     filter(year == 2022) %>%
  #     select(id, 
  #            height_change_2022 = height_change,
  #            rcd_change_2022 = rcd_change),
  #   by = "id"
  # ) %>%
  # 
  # # conditionally replace 2023 negatives
  # mutate(
  #   height_change = if_else(
  #     year == 2023 &
  #       height_change < 0, 
  #       # herbivory == 1 &,
  #     height_change_2022,
  #     height_change
  #   )
  #  ) %>%
  # select(-c(height_change_2022, rcd_change_2022)) %>%


  # add the 15N data
  
  inner_join(
    read_csv("outplut/combined_soil_leaf_with_myc_type_and_enrichment.csv") %>%
      
      # clean data 
      
      clean_colnames() %>%
      mutate(number = as.numeric(tree_number)) %>%
      select(-c(soil_lab_id, id_number, soil_sample_weight, soil_plate_pos, g_id_number, duplicate, tree_number,`___17`,leaf_lab_id,leaf_plate_pos)) ,
    
    by = c("unit", "plot", "species", "number")
  ) %>%
  
  # remove the tiam data as so many values were lost
  
  filter(species != "tiam") %>%
  
  na.omit() %>%
  
  # add biomass calculations
  
  left_join(
    #create dataset for greenbiomass from Miles and Smith 2009
    tibble(species = c("acru","acsa","nysy", "prse","bele","caco","quru","tiam"),
           sggw = c(0.49,0.56,0.46,0.47,0.6,0.6,0.56,0.32) ),
    by = "species"
  ) %>%
  
  # planting dates
  
  left_join(
    planting_dates, 
    by = c("unit", "plot", "year")
  ) %>%
 
  # calculate growth 
  
  mutate(
    biomass = (3.14*((rcd_mm/10)^2)*(ht_cm/3))*sggw*1,
    biomass_initial = (3.14*((rcd_mm_initial/10)^2)*(ht_cm_initial/3))*sggw*1,
    biomass_growth = (log(biomass) - log(biomass_initial))/time_months
  ) %>%
  
  # add scaled values for th brms models
  
  mutate(
    n15_s = scale(foliar_15n_enrichment)[,1],
    leafn_s = scale(leaf_percent_n)[,1],
    herbivory_s = scale(herbivory)[,1],
  ) %>%
  
  write_csv("outplut/alldata_03262026.csv")


###
# Growth and isotope visualizations  ----------------------------------------------

# by seedling

seedling_data %>%
  ggplot(aes(foliar_15n_enrichment, biomass_growth, color = species)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = species_palette) +
  geom_smooth(method = "gam") +
  my_theme() + 
  facet_wrap(~year, scales = "free")

# by mycorrhizal type

seedling_data %>%
  ggplot(aes(foliar_15n_enrichment, biomass_growth, color = myc_type)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c(blues[5], oranges[5])) +
  geom_smooth(method = "gam") +
  my_theme() + 
  facet_wrap(vars(year, mycorrhizal_legacy), scales = "free")

# seedling_data %>%

seedling_data %>%
ggplot(aes(leaf_percent_n, biomass_growth, color = myc_type)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c(blues[5], oranges[5])) +
  geom_smooth(method = "gam") +
  my_theme() + 
  facet_wrap(vars(year, mycorrhizal_legacy), scales = "free")

# seedling_data %>%

seedling_data %>%
  filter(year == "2023") %>%
  ggplot(aes(leaf_percent_n, foliar_15n_enrichment, color = myc_type)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c(blues[5], oranges[5])) +
  geom_smooth(method = "gam") +
  my_theme() + 
  facet_wrap(~mycorrhizal_legacy)

# am seedlings in EcM plots have more nitrogen when it is more "mycorrhizal derived"




# PCA for growth - using height, rcd, herbivory data ----------------------

pca_growth <- 
  seedling_data %>%
  select(height_change, rcd_change, ht_cm_initial, rcd_mm_initial, herbivory, condition) %>%
  prcomp(scale = TRUE, center = TRUE)

# variance explained

summary(pca_growth)$importance["Proportion of Variance", 1:3] # 33 and 25  for 57% total variation explained

# visualize

factoextra::fviz_pca_var(pca_growth,
                         col.var = "contrib", # Color by contributions to the PC
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         # Avoid text overlapping
                         repel = TRUE )


# add PC axes 
seedling_data %>%
  
mutate(PC1 = pca_growth$x[,1] , 
       PC2 = pca_growth$x[,2])
  
  




# Bayesian analyses for growth ~ 15N ---------------------------------------------------------

mod_1 <- 
  brms::brm(biomass_growth ~ n15_s + leafn_s + mycorrhizal_legacy + myc_type + herbivory_s + (1 | site_unit) + (1 | species),
            data = seedling_data %>%
              filter(year == "2023"))

summary(mod_1)

mod_2 <- 
  brms::brm(height_change ~ n15_s + leafn_s + mycorrhizal_legacy + myc_type + herbivory_s + (1 | site_unit) + (1 | species),
            data = seedling_data %>%
              filter(year == "2023"))

summary(mod_2)

# test for interactions

mod_3 <- 
  brms::brm(height_change ~ n15_s + leafn_s + mycorrhizal_legacy + myc_type + herbivory_s + (1 | site_unit) + (1 | species),
            data = seedling_data %>%
              filter(year == "2023"))

summary(mod_2)


# Frequentist modeling for relationship between nitrogen and seedl --------

mod_2 <- 
  lme4::lmer(height_change ~ n15_s + leafn_s + mycorrhizal_legacy + myc_type
           + (1 | site_unit) + (1 | species) + (1 | site_unit/species) ,
            data = seedling_data %>%
              filter(year == "2023"))

# issues with model convergence

summary(mod_2)

# Analyses for change in growth increment over time -----------------------



  
  
