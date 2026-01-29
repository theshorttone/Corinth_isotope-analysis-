#Data Sheet combination
#08/22/2025

# librarys  ----------------------------------------------------------------

library(tidyverse)



# data import -------------------------------------------------------------

leaf_data_filepath <- "data/corinth_leaf_2024.csv"
soil_data_filepath <- "data/corinth_soil_2024.csv"

df_leaf <- read_csv(leaf_data_filepath)
df_soil <- read_csv(soil_data_filepath)


# data wrangling ----------------------------------------------------------

df_leaf_clean <- df_leaf %>% 
  rename(
    sample_id = `Sample ID`,
    sample_weight = `Amount (mg)`,
    percent_c = `%C`,
    percent_n = `%N`,
    d13c = d13C,
    d15n = d15N
  ) %>% 
  filter(!str_detect(sample_id, "peach|cocoa|blank"))
