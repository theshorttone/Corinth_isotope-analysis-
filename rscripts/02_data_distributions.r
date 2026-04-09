#try doing boxplots instead for distribution 


# Setup -------------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(RColorBrewer)

blues   <- RColorBrewer::brewer.pal(9, "Blues")
oranges <- RColorBrewer::brewer.pal(9, "Oranges")

species_palette <- c(
  "PRSE" = blues[3],
  "ACSA" = blues[5],
  "ACRU" = blues[7],
  "NYSY" = blues[9],
  "BELE" = oranges[3],
  "QURU" = oranges[5],
  "TIAM" = oranges[7],
  "CACO" = oranges[9]
)
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


# load files --------------------------------------------------------------

isotope_data <-
  read.csv("outplut/combined_soil_leaf") %>% 
  mutate(
    site_unit = factor(paste(unit, plot, sep = "_")),
        mycorrhizal_legacy = case_when(
          site_unit == "2_5" ~ "ecm",
          site_unit == "2_9"  ~ "am",
          site_unit == "3_5"  ~ "am",
          site_unit == "8_1" ~ "am",
          site_unit == "10_5" ~ "am",
          site_unit == "14_5" ~ "ecm",
          site_unit == "14_6" ~ "ecm",
          site_unit == "14_7" ~ "ecm",
          TRUE ~ "none"
        ),
    myc_type = case_when(
      species %in% c("PRSE", "ACSA", "ACRU", "NYSY") ~ "am",
      species %in% c("BELE", "QURU", "TIAM", "CACO") ~ "ecm",
      TRUE ~ "none"),
    species = factor(species, levels = names(species_palette)),
    distance_to_edge_m = as.numeric(distance_to_edge_m)
      ) 

  #write.csv(isotope_data, "outplut/combined_soil_leaf_with_myc_type", row.names = FALSE)


  
  



# 15N distributions -------------------------------------------------------
filter(isotope_data, !is.na(leaf_d15n)) %>%
  ggplot(aes(x = leaf_d15n)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Distribution of Leaf δ15N Values",
    x = "Leaf δ15N (‰)",
    y = "Frequency"
  )

# 13C distributions --------------------------------------------------------
filter(isotope_data, !is.na(leaf_d13c)) %>%
  ggplot(aes(x = leaf_d13c)) +
  geom_histogram(binwidth = 0.5) +
  labs(
    title = "Distribution of Leaf δ13C Values",
    x = "Leaf δ13C (‰)",
    y = "Frequency"
  ) +
  my_theme()


# Isotope calculations ----------------------------------------------------


# Calculate the foliar 15N enrichment (εp/s = δ15N_foliage − δ15N_soil) following Cheng et al. 2010.
isotope_data <- isotope_data %>%
  mutate(
    foliar_15N_enrichment = leaf_d15n - soil_d15n
  ) 
  
  write_csv(isotope_data, "outplut/combined_soil_leaf_with_myc_type_and_enrichment.csv")
    
    

# Plot foliar 15N enrichment distribution ----------------------------------

filter(isotope_data, !is.na(foliar_15N_enrichment)) %>%
  ggplot(aes(x = foliar_15N_enrichment, color = location)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Distribution of Foliar 15N Enrichment (εp/s)",
    x = "Foliar 15N Enrichment (‰)",
    y = "Frequency"
  )

filter(isotope_data, !is.na(foliar_15N_enrichment)) %>%
  ggplot(aes(x = foliar_15N_enrichment, fill = species)) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = species_palette)+
  labs(
    title = "Distribution of Foliar 15N Enrichment (εp/s)",
    x = "Foliar 15N Enrichment (‰)",
    y = "Frequency"
  )



# foliar enrichment by vars -----------------------------------------------

filter(isotope_data, !is.na(foliar_15N_enrichment)) %>%
  ggplot(aes(x = location, y = foliar_15N_enrichment, color = species)) +
  geom_boxplot() +
  scale_color_manual(values = species_palette)+
  labs(
    x = "Location",
    y = "Foliar 15N Enrichment (‰)"
  )+
  theme_few()

#foliar_enrichment by legacy

filter(isotope_data, !is.na(foliar_15N_enrichment)) %>%
  ggplot(aes(x = mycorrhizal_legacy, y = foliar_15N_enrichment, color = species)) +
  geom_boxplot() +
  scale_color_manual(values = species_palette)+
  labs(
    x = "plot mycorrhizal legacy",
    y = "Foliar 15N Enrichment (‰)"
  )+
  theme_few()

ggsave("outplut/figures/foliar_15N_enrichment_bar_plot_by_species.png", width = 8, height = 6)


filter(isotope_data, !is.na(foliar_15N_enrichment)) %>%
  ggplot(aes(x = distance_to_edge_m, y = foliar_15N_enrichment, color = species))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values = species_palette)+
  facet_wrap(~mycorrhizal_legacy,
             labeller = as_labeller(c(
               "am" = "AM Legacy Plots",
               "ecm" = "EcM Legacy PLots"
             )))+
  labs(
    y = "Foliar 15N Enrichment",
    x = "Distance to plot edge (m)",
    color = "Tree Species"
  ) +
  theme_few()

ggsave("outplut/figures/foliar_15N_enrichment_vs_distance_to_edge_species.png", width = 8, height = 6)

filter(isotope_data, !is.na(foliar_15N_enrichment)) %>%
  ggplot(aes(x = distance_to_edge_m, y = leaf_percent_n, color = species))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values = species_palette)+
  facet_wrap(~mycorrhizal_legacy,
             labeller = as_labeller(c(
               "am" = "AM Legacy Plots",
               "ecm" = "EcM Legacy PLots"
             )))+
  labs(
    y = "leaf_percent_n",
    x = "Distance to plot edge (m)",
    color = "Tree Species"
  ) +
  theme_few()

ggsave("outplut/figures/foliar_15N_enrichment_vs_distance_to_edge_species.png", width = 8, height = 6)




filter(isotope_data, !is.na(foliar_15N_enrichment)) %>%
  ggplot(aes(x = distance_to_edge_m, y = foliar_15N_enrichment, color = myc_type))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values = c(blues[5], oranges[5]))+
  facet_wrap(~mycorrhizal_legacy,
             labeller = as_labeller(c(
               "am" = "AM Legacy Plots",
               "ecm" = "EcM Legacy PLots"
             )))+
  labs(
    y = "Foliar 15N Enrichment",
    x = "Distance to plot edge (m)",
    color = "Mycorrhizal Type"
  ) +
  theme_few()

ggsave("outplut/figures/foliar_15N_enrichment_vs_distance_to_edge_myc_type.png", width = 8, height = 6)
  




