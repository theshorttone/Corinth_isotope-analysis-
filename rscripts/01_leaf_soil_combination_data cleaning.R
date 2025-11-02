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
    leaf_lab_id = `OurLabID`,
    leaf_sample_weight = `Amount (mg)`,
    lead_percent_c = `%C`,
    leaf_percent_n = `%N`,
    leaf_d13c = d13C,
    leaf_d15n = d15N,
    leaf_plate_pos = `Plate Position`
  ) %>% 
  filter(!str_detect(sample_id, "peach|cocoa|blank")) %>% 
  separate(sample_id, into = c("tree_number", "unit_plot", "location", "species"), sep = " ") %>%
  separate(unit_plot, into = c("unit", "plot"), sep = "_") %>% 
  select(-location)

df_soil_clean <- df_soil %>% 
  select(-((ncol(.)-7):ncol(.))) %>% 
  rename(
    g_id_number = `Sample ID`,
    soil_lab_id = `OurLabID`,
    soil_sample_weight = `Amount (mg)`,
    soil_percent_c = `%C`,
    soil_percent_n = `%N`,
    soil_d13c = d13C,
    soil_d15n = d15N,
    soil_comments = Comments,
    soil_plate_pos = `Plate position`
  ) %>% 
  filter(!str_detect(g_id_number, "AL")) %>% 
  #####filter out by bad soil comments ######
  filter(is.na(soil_comments) | !str_detect(soil_comments, "Nitrogen QA/QC did not pass")) %>% 
  filter(is.na(soil_comments) | !str_detect(soil_comments, "soil debris in transfer plate - %C and %N values may not be accurate")) %>%
  filter(is.na(soil_comments) | !str_detect(soil_comments, "Too much carbon in sample - saturated detectors")) %>% 
  mutate(
    id_number = str_remove(g_id_number, "^[0-9]+G_"),
    id_clean = str_remove(id_number, "_.*$"),
    duplicate = duplicated(id_clean) | duplicated(id_clean, fromLast = TRUE)) %>% 
  relocate(id_clean, .after = 1) %>%      # move id_number to column 2
  relocate(duplicate, .after = id_clean) %>% 
  select (-g_id_number, 
          -soil_comments)
  


# average soil duplicates -------------------------------------------------

df_soil_clean_no_dup <- df_soil_clean %>%
  group_by(id_clean) %>%                          # group duplicates together
  summarize(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),   # average numeric columns
    across(where(is.character), ~ str_c(unique(.x), collapse = " and ")),  # concat strings
    duplicate = any(duplicate),                    # keep TRUE if any were duplicates
    .groups = "drop"
  )
 
# combine soil and leaf ---------------------------------------------------

norm_keys <- function(df) {
  df %>%
    mutate(
      unit        = as.character(.data$unit),
      plot        = as.character(.data$plot),
      tree_number = as.character(.data$tree_number),
      species     = str_to_upper(.data$species)
    )
}

key_df  <- norm_keys(def_key)
df_leaf_m <- norm_keys(df_leaf_clean)

df_c <- df_leaf_m %>%
  full_join(
    key_df,
    by = dplyr::join_by(unit, plot, species, tree_number)
  ) %>% 
  group_by(id) %>%                          # group duplicates together
  summarize(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),   # average numeric columns
    across(where(is.character), ~ str_c(unique(.x), collapse = " and ")),  # concat strings
    .groups = "drop"
  )
  

# final merge -------------------------------------------------------------


df_final <- df_c %>%
  mutate(id = as.character(id)) %>%                       
  full_join(
    df_soil_clean_no_dup %>%
      mutate(id_clean = as.character(str_trim(id_clean))),
    by = dplyr::join_by(id == id_clean)
  ) %>% 
  relocate(id_number, .after = 1) %>%      
  relocate(tree_number, .after = 2) %>% 
  relocate(unit, .after = 3) %>% 
  relocate(plot, .after = 4) %>% 
  relocate(species, .after = 5) %>% 
  relocate(location, .after = 6)
  


  

  
  
  
  
  


