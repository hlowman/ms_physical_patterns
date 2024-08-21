# Figures Script
# August 21, 2024
# Heili Lowman

#### READ ME ####

# The following script will be used to generate figures
# of all kinds during the progression of the project.

#### Setup ####

# Load packages.
library(here)
library(tidyverse)
library(ggplot2)
library(naniar)
library(lubridate)
library(macrosheds)
library(patchwork)

# Load datasets.
load(file.path('data_raw', 'ms_site_data.RData')) # site metadata
hydro_trend_data <- read_csv("data_working/hydro_climate_trends.csv") # hydroclimate trends

#### Boxplot of trends ####

# Create a figure to summarize the significant, and non-significant,
# trends in hydroclimatic variables (q, precip, temp, pH).

# First, need to denote significant and non-significant trends.
hydro_trend_data <- hydro_trend_data %>%
  mutate(sig = case_when(p < 0.05 ~ "YES",
                         TRUE ~ "NO"))

# Next, need to append regions to the trends.
hydro_data <- left_join(hydro_trend_data, ms_site_data)

# And remove metrics we are not interested in.
hydro_data <- hydro_data %>%
  dplyr::filter(!var %in% c("a_flow_sig", "b_flow_sig"))

# And add some more names to the NEON sites.
hydro_data <- hydro_data %>%
  mutate(domain_specificname = case_when(site_fullname %in% c("Arikaree River", "Upper Big Creek", 
                                                              "Blacktail Deer Creek", "Blue River", 
                                                              "Black Warrior River near Dead Lake", 
                                                              "Como Creek", "Flint River",
                                                              "Lower Hop Brook", "Kings Creek",
                                                              "LeConte Creek", "Lewis Run",
                                                              "Martha Creek", "Mayfield Creek",
                                                              "McDiffett Creek", "McRae Creek",
                                                              "Posey Creek", "Pringle Creek",
                                                              "Red Butte Creek", "Sycamore Creek",
                                                              "Teakettle 2 Creek", 
                                                              "Lower Tombigbee River at Choctaw Refuge",
                                                              "Walker Branch", "West St Louis Creek") ~ site_fullname,
                                         site_fullname %in% c("Bigelow Zero Order Basin",
                                                              "Marshall Gulch Schist catchment",
                                                              "Oracle Ridge") ~ "Santa Catalina",
                                         site_fullname %in% c("History Grove",
                                                              "La Jara South Spring",
                                                              "Lower Jaramillo Creek",
                                                              "Lower La Jara Creek",
                                                              "Lower Redondo Creek",
                                                              "MC ZOB Flume",
                                                              "Redondo Meadow",
                                                              "Upper Jaramillo Creek",
                                                              "Upper Jaramillo Spring", 
                                                              "Upper La Jara Creek", 
                                                              "Upper Redondo Creek") ~ "Jemez",
                                         site_fullname == "USGS" ~ "Black Earth Creek",
                                         is.na(site_fullname) ~ "Unavailable",
                                         TRUE ~ domain_fullname))

# Finally create a boxplot with jittered points over top.
(fig_boxplot_tempann <- ggplot(hydro_data %>% 
                                 filter(sig == "YES") %>%
                                 filter(var == "temp_mean_ann"), 
                                 aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "temp_mean_ann"),
                color = "grey70", alpha = 0.3) +
    xlim(-0.06, 0.06) +
    labs(y = "Mean T", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_precipann <- ggplot(hydro_data %>% 
                                   filter(sig == "YES") %>%
                                   filter(var == "precip_mean_ann"), 
                          aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "precip_mean_ann"),
                color = "grey70", alpha = 0.3) +
    xlim(-0.06, 0.06) +
    labs(y = "Mean P", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_preciptot <- ggplot(hydro_data %>% 
                                   filter(sig == "YES") %>%
                                   filter(var == "precip_total_ann"), 
                                 aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "precip_total_ann"),
                color = "grey70", alpha = 0.3) +
    xlim(-15, 15) +
    labs(y = "Tot. P", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_m1 <- ggplot(hydro_data %>% 
                            filter(sig == "YES") %>%
                            filter(var == "m1_meanq"), 
                          aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "m1_meanq"),
                color = "grey70", alpha = 0.3) +
    xlim(-0.5, 0.5) +
    labs(y = "Mean Q", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_m2 <- ggplot(hydro_data %>% 
                            filter(sig == "YES") %>%
                            filter(var == "m2_cvq"), 
                          aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "m2_cvq"),
                color = "grey70", alpha = 0.3) +
    xlim(-0.5, 0.5) +
    labs(y = "C.V. Q", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_m3 <- ggplot(hydro_data %>% 
                            filter(sig == "YES") %>%
                            filter(var == "m3_skewq"), 
                          aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "m3_skewq"),
                color = "grey70", alpha = 0.3) +
    xlim(-0.5, 0.5) +
    labs(y = "Skew Q", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_m4 <- ggplot(hydro_data %>% 
                            filter(sig == "YES") %>%
                            filter(var == "m4_kurtq"), 
                          aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "m4_kurtq"),
                color = "grey70", alpha = 0.3) +
    xlim(-10, 10) +
    labs(y = "Kurt. Q", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_m5 <- ggplot(hydro_data %>% 
                            filter(sig == "YES") %>%
                            filter(var == "m5_ar1q"), 
                          aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "m5_ar1q"),
                color = "grey70", alpha = 0.3) +
    xlim(-0.06, 0.06) +
    labs(y = "AR(1) Q", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_m6 <- ggplot(hydro_data %>% 
                            filter(sig == "YES") %>%
                            filter(var == "m6_ampq"), 
                          aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "m6_ampq"),
                color = "grey70", alpha = 0.3) +
    xlim(-0.06, 0.06) +
    labs(y = "Amp. Q", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_m7 <- ggplot(hydro_data %>% 
                            filter(sig == "YES") %>%
                            filter(var == "m7_phiq"), 
                          aes(x = trend, y = var)) +
    geom_boxplot(fill = NA, color = "black") +
    geom_jitter(color = "black", alpha = 0.8) +
    geom_jitter(data = hydro_data %>% 
                  filter(sig == "NO") %>%
                  filter(var == "m7_phiq"),
                color = "grey70", alpha = 0.3) +
    xlim(-0.2, 0.2) +
    labs(x = "Trend Value", y = "Phi Q", color = "Domain") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none"))

(fig_boxplot <- fig_boxplot_tempann /
    fig_boxplot_precipann /
    fig_boxplot_preciptot /
    fig_boxplot_m1 /
    fig_boxplot_m2 /
    fig_boxplot_m3 /
    fig_boxplot_m4 /
    fig_boxplot_m5 /
    fig_boxplot_m6 / 
    fig_boxplot_m7)

# ggsave(plot = fig_boxplot,
#        filename = "figures/hydro_trends_boxplots_082124.jpg",
#        width = 15,
#        height = 20,
#        units = "cm")

# Quick summary table.
trend_summary <- hydro_data %>%
  count(var, sig) %>%
  ungroup() %>%
  pivot_wider(names_from = sig, values_from = n) %>%
  mutate(sum = NO + YES)

# End of script.
