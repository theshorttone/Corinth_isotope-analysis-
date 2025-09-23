#Data Sheet combination
#08/22/2025

# librarys  ----------------------------------------------------------------

library(tidyverse)



# data import -------------------------------------------------------------

leaf_data_filepath <- "data/corinth_leaf_2024.csv"
soil_data_filepath <- "data/corinth_soil_2024.csv"
dist_to_edge_filepath <- "data/corinth_dist_to_edge.csv"


df_leaf <- read_csv(leaf_data_filepath)
df_soil <- read_csv(soil_data_filepath)
def_key <- read_csv(dist_to_edge_filepath)


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
  filter(!str_detect(sample_id, "peach|cocoa|blank")) %>% 
  separate(sample_id, into = c("tree_number", "unit_plot", "location", "species"), sep = " ") %>%
  separate(unit_plot, into = c("unit", "plot"), sep = "_")

df_soil_clean <- df_soil %>% 
  select(-((ncol(.)-7):ncol(.))) %>% 
  rename(
    g_id_number = `Sample ID`,
    sample_weight = `Amount (mg)`,
    percent_c = `%C`,
    percent_n = `%N`,
    d13c = d13C,
    d15n = d15N,
    comments = Comments
  ) %>% 
  filter(!str_detect(g_id_number, "AL")) %>% 
  mutate(
    id_number = str_remove(g_id_number, "^[0-9]+G_"),
    duplicate = str_detect(id_number, "_DUP\\??$"),    
    id_clean = str_remove(id_number, "_DUP\\??$")) %>% 
  filter(is.na(comments) | !str_detect(comments, "Nitrogen QA/QC did not pass"))
  
  
  


