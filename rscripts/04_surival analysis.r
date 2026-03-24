# Amelia Fitch
# seedling survival analyses

# setup  ----------------------------------------------------------------

library(tidyverse)


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


# data import -------------------------------------------------------------

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
    read_csv("data/soil_data_combined.csv") %>%
      
      # clean data 
      
      clean_colnames() %>%
      mutate(number = as.numeric(tree_number)) %>%
      select(-c(soil_lab_id, id_number, id_clean, soil_sample_weight, soil_plate_pos, g_id_number, duplicate, tree_number,`___18`)),
    
    by = c("unit", "plot", "species", "number")
  )



# Survival curve calculation ----------------------------------------------

seedling_data %>%
  
  ggplot(aes(height_change, soil_d15n))

 
  

  
  
