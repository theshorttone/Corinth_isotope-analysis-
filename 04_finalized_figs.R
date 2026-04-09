#Zachary Shortt April 9 2026 
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

isotope_data <- read_csv("outplut/combined_soil_leaf_with_myc_type_and_enrichment.csv")
isotope_data <- isotope_data %>% 
filter (species != "TIAM")

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
    species = toupper(species)
  )


# Main Foliar enrichment figs -----------------------------------------------

#matches model 1, height is a function of dist from edge 
#and mycorrhizal type but not myc legacy

filter(myc_2023, !is.na(foliar_15n_enrichment)) %>%
  ggplot(aes(x = leaf_percent_n, y = height_change, color = myc_type))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values = c(blues[5], oranges[5]))+
  facet_wrap(~mycorrhizal_legacy,
             labeller = as_labeller(c(
               "am" = "AM Legacy Plots",
               "ecm" = "EcM Legacy PLots"
             )))+
  labs(
    y = "2023 Height change (cm)",
    x = "leaf_percent_n",
    color = "Mycorrhizal Type"
  )+
  theme_few()

#matches model 2, leaf percent N is a function of distance to edge (though myc type 
#and plot weren't included in model, should I simplify?)

filter(isotope_data, mycorrhizal_legacy %in% c("am", "ecm")) %>% 
  ggplot(aes(x = distance_to_edge_m, y = leaf_percent_n, color = myc_type))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values = c(blues[5], oranges[5]))+
  facet_wrap(~mycorrhizal_legacy,
             labeller = as_labeller(c(
               "am" = "AM Legacy Plots",
               "ecm" = "EcM Legacy PLots"
             )))+
  labs(
    y = "Leaf Percent N",
    x = "Distance to plot edge (m)",
    color = "Mycorrhizal Type"
  )+
  theme_few()

#matches model 3, foliar 15N enrichment is a function of distance to edge and 
#interaction between dist to edge and mycorrhizal type and  
#almost interaction between distance to edge and legacy plot 

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
  )+
  theme_few()

ggsave("outplut/figures/foliar_15N_enrichment_vs_distance_to_edge_myc_type.png", width = 8, height = 6)


# By Species for supplementary  -------------------------------------------

filter(myc_2023, !is.na(foliar_15n_enrichment)) %>%
  ggplot(aes(x = distance_to_edge_m, y = height_change, color = species))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values = species_palette)+
  facet_wrap(~mycorrhizal_legacy,
             labeller = as_labeller(c(
               "am" = "AM Legacy Plots",
               "ecm" = "EcM Legacy PLots"
             )))+
  labs(
    y = "Height change (cm?)",
    x = "Distance to Edge (m)",
    color = "Mycorrhizal Type"
  )+
  theme_few()



filter(myc_2023, !is.na(foliar_15n_enrichment)) %>%
  ggplot(aes(x = height_change, y = foliar_15n_enrichment, color = species
             ))+
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
    x = "height change",
    color = "Mycorrhizal Type"
  )+
  theme_few()


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
  )+
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
  )+
  theme_few()

ggsave("outplut/figures/foliar_15N_enrichment_vs_distance_to_edge_species.png", width = 8, height = 6)


filter(isotope_data, !is.na(soil_percent_n)) %>%
  ggplot(aes(x = distance_to_edge_m, y = soil_percent_n, color = myc_type))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_manual(values = c(blues[5], oranges[5]))+
  facet_wrap(~mycorrhizal_legacy,
             labeller = as_labeller(c(
               "am" = "AM Legacy Plots",
               "ecm" = "EcM Legacy PLots"
             )))+
  labs(
    y = "soil_percent_n",
    x = "Distance to plot edge (m)",
    color = "Mycorrhizal Type"
  )+
  theme_few()

