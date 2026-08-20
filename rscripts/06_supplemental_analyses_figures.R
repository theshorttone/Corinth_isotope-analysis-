# 07/30/2026, Amelia Fitch


# setup -------------------------------------------------------------------
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

# read in files

pre_harvest_data <- 
  read_csv("data/Corinth.alldata.csv") %>%
  clean_colnames() %>%
  
  # calculate CN
  # unit_plot for random effect
  
  mutate(cn_ratio = per_c/per_n,
         unit_plot = paste(unit, plot, sep = "_"))



# C:N ratio analysis ---------------------------------------

pre_harvest_data %>%
  filter(treatment == "cut") %>%
  ggplot(aes(x = mt, y = cn_ratio)) +
  geom_boxplot() +
  my_theme() +
  # facet_wrap(~ site) +
  labs(y = "Soil C:N ratio",
       x = "Mycorrhizal legacy type") 


# analyses

mod_cn <- nlme::lme(cn_ratio ~ mt, random = ~1 | unit_plot, data = pre_harvest_data)
summary(mod_cn)
car::Anova(mod_cn)

emmeans::emmeans(mod_cn ,~ mt)
# emmeans::emtrends(mod_cn ,pairwise ~ mt, var = "mt")

# Site information table --------------------------------------------------

# summarize the replicates within plot 


