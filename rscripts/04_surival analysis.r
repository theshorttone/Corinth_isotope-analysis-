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
         rcd_change = rcd_mm - rcd_mm_initial) %>%


  # add the 15N data
  
  inner_join(
    read_csv("outplut/combined_soil_leaf_with_myc_type_and_enrichment.csv") %>%
      
      # clean data 
      
      clean_colnames() %>%
      mutate(number = as.numeric(tree_number)) %>%
      select(-c(soil_lab_id, id_number, soil_sample_weight, soil_plate_pos, g_id_number, duplicate, tree_number,`___17`,leaf_lab_id,leaf_plate_pos)) ,
    
    by = c("unit", "plot", "species", "number")
  )



# Growth and isotope visualizations  ----------------------------------------------

# by seedling

seedling_data %>%
  ggplot(aes(foliar_15n_enrichment, height_change, color = species)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = species_palette) +
  geom_smooth(method = "lm") +
  my_theme() + 
  facet_wrap(~year, scales = "free")

# by mycorrhizal type

seedling_data %>%
  ggplot(aes(foliar_15n_enrichment, height_change, color = myc_type)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c(blues[5], oranges[5])) +
  geom_smooth(method = "lm") +
  my_theme() + 
  facet_wrap(vars(year, mycorrhizal_legacy), scales = "free")

# seedling_data %>%

seedling_data %>%
ggplot(aes(leaf_percent_n, height_change, color = myc_type)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c(blues[5], oranges[5])) +
  geom_smooth(method = "lm") +
  my_theme() + 
  facet_wrap(vars(year, mycorrhizal_legacy), scales = "free")

# seedling_data %>%

seedling_data %>%
  filter(year == "2023") %>%
  ggplot(aes(leaf_percent_n, foliar_15n_enrichment, color = myc_type)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c(blues[5], oranges[5])) +
  geom_smooth(method = "lm") +
  my_theme() + 
  facet_wrap(~mycorrhizal_legacy)

# am seedlings in EcM plots have more nitrogen when it is more "mycorrhizal derived"


# Analyses for growth ~ 15N ---------------------------------------------------------

mod_1 <- 
  brms::brm(height_change ~ foliar_15n_enrichment * myc_type * mycorrhizal_legacy + (1 | plot) + (1 | species),
            data = seedling_data %>%
              filter(year == "2023"))

# Analyses for change in growth increment over time -----------------------

# gam models allow for non-linearity over time 

mgcv::gam(height_change ~ year * myc_type * mycorrhizal_legacy + (1 | plot/seedling))

  
  
