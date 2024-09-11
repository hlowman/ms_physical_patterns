# Figures Script
# August 21, 2024
# Heili Lowman

#### READ ME ####

# The following script will be used to generate figures
# of all kinds during the progression of the project.

#### Setup ####

# Load packages.
library(here)
source(here('src', 'setup.R'))

# Load trend dataset.
hydro_trend_data <- read_csv("data_working/hydro_climate_trends.csv")

# Load chemistry dataset - be patient, takes just a moment.
chem_data <- ms_load_product(
    macrosheds_root = here("data_raw"),
    prodname = "stream_chemistry",
    warn = F)

# Load climate dataset - be truly patient, takes a minute or two.
clim <- read_feather(here('data_raw', 'spatial_timeseries_climate.feather')) %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean)

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
# Not using this currently, but keeping here for future reference.
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
    xlim(-0.15, 0.15) +
    labs(y = "Mean T") +
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
    xlim(-0.2, 0.2) +
    labs(y = "Mean P") +
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
    xlim(-75, 75) +
    labs(y = "Tot. P") +
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
    xlim(-0.3, 0.3) +
    labs(y = "Mean Q") +
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
    xlim(-0.2, 0.2) +
    labs(y = "C.V. Q") +
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
    xlim(-0.6, 0.6) +
    labs(y = "Skew Q") +
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
    labs(y = "Kurt. Q") +
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
    xlim(-0.03, 0.03) +
    labs(y = "AR(1) Q") +
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
    xlim(-0.08, 0.08) +
    labs(y = "Amp. Q") +
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
    xlim(-0.5, 0.5) +
    labs(y = "Phi Q") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"))

(fig_boxplot_rbi <- ggplot(hydro_data %>%
                              filter(sig == "YES") %>%
                              filter(var == "rbiq"),
                          aes(x = trend, y = var)) +
        geom_boxplot(fill = NA, color = "black") +
        geom_jitter(color = "black", alpha = 0.8) +
        geom_jitter(data = hydro_data %>%
                        filter(sig == "NO") %>%
                        filter(var == "rbiq"),
                    color = "grey70", alpha = 0.3) +
        xlim(-0.02, 0.02) +
        labs(x = "Trend Value", y = "RBI Q") +
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
    fig_boxplot_m7 /
    fig_boxplot_rbi)

# ggsave(plot = fig_boxplot,
#        filename = "figures/hydro_trends_boxplots.jpg",
#        width = 15,
#        height = 22,
#        units = "cm")

# Making a few more boxplots for the discharge quantiles
(fig_boxplot_q1 <- ggplot(hydro_data %>%
                              filter(sig == "YES") %>%
                              filter(var == "q1"),
                          aes(x = trend, y = var)) +
        geom_boxplot(fill = NA, color = "black") +
        geom_jitter(color = "black", alpha = 0.8) +
        geom_jitter(data = hydro_data %>%
                        filter(sig == "NO") %>%
                        filter(var == "q1"),
                    color = "grey70", alpha = 0.3) +
        xlim(-0.04, 0.04) +
        labs(y = "1st Q") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none"))

(fig_boxplot_q5 <- ggplot(hydro_data %>%
                              filter(sig == "YES") %>%
                              filter(var == "q5"),
                          aes(x = trend, y = var)) +
        geom_boxplot(fill = NA, color = "black") +
        geom_jitter(color = "black", alpha = 0.8) +
        geom_jitter(data = hydro_data %>%
                        filter(sig == "NO") %>%
                        filter(var == "q5"),
                    color = "grey70", alpha = 0.3) +
        xlim(-0.06, 0.06) +
        labs(y = "5th Q") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none"))

(fig_boxplot_q25 <- ggplot(hydro_data %>%
                              filter(sig == "YES") %>%
                              filter(var == "q25"),
                          aes(x = trend, y = var)) +
        geom_boxplot(fill = NA, color = "black") +
        geom_jitter(color = "black", alpha = 0.8) +
        geom_jitter(data = hydro_data %>%
                        filter(sig == "NO") %>%
                        filter(var == "q25"),
                    color = "grey70", alpha = 0.3) +
        xlim(-0.1, 0.1) +
        labs(y = "25th Q") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none"))

(fig_boxplot_q50 <- ggplot(hydro_data %>%
                              filter(sig == "YES") %>%
                              filter(var == "q50"),
                          aes(x = trend, y = var)) +
        geom_boxplot(fill = NA, color = "black") +
        geom_jitter(color = "black", alpha = 0.8) +
        geom_jitter(data = hydro_data %>%
                        filter(sig == "NO") %>%
                        filter(var == "q50"),
                    color = "grey70", alpha = 0.3) +
        xlim(-0.1, 0.1) +
        labs(y = "Median Q") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none"))

(fig_boxplot_q75 <- ggplot(hydro_data %>%
                              filter(sig == "YES") %>%
                              filter(var == "q75"),
                          aes(x = trend, y = var)) +
        geom_boxplot(fill = NA, color = "black") +
        geom_jitter(color = "black", alpha = 0.8) +
        geom_jitter(data = hydro_data %>%
                        filter(sig == "NO") %>%
                        filter(var == "q75"),
                    color = "grey70", alpha = 0.3) +
        xlim(-0.3, 0.3) +
        labs(y = "75th Q") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none"))


(fig_boxplot_q95 <- ggplot(hydro_data %>%
                               filter(sig == "YES") %>%
                               filter(var == "q95"),
                           aes(x = trend, y = var)) +
        geom_boxplot(fill = NA, color = "black") +
        geom_jitter(color = "black", alpha = 0.8) +
        geom_jitter(data = hydro_data %>%
                        filter(sig == "NO") %>%
                        filter(var == "q95"),
                    color = "grey70", alpha = 0.3) +
        xlim(-1.5, 1.5) +
        labs(y = "95th Q") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              legend.position = "none"))

(fig_boxplot_q99 <- ggplot(hydro_data %>%
                               filter(sig == "YES") %>%
                               filter(var == "q99"),
                           aes(x = trend, y = var)) +
        geom_boxplot(fill = NA, color = "black") +
        geom_jitter(color = "black", alpha = 0.8) +
        geom_jitter(data = hydro_data %>%
                        filter(sig == "NO") %>%
                        filter(var == "q99"),
                    color = "grey70", alpha = 0.3) +
        xlim(-3, 3) +
        labs(x = "Trend Value", y = "99th Q") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none"))

(fig_Qboxplot <- fig_boxplot_q1 /
        fig_boxplot_q5 /
        fig_boxplot_q25 /
        fig_boxplot_q50 /
        fig_boxplot_q75 /
        fig_boxplot_q95 /
        fig_boxplot_q99)

# ggsave(plot = fig_Qboxplot,
#        filename = "figures/hydro_trends_Qboxplots.jpg",
#        width = 15,
#        height = 15,
#        units = "cm")

# Quick summary table.
trend_summary <- hydro_data %>%
  count(var, sig) %>%
  ungroup() %>%
  pivot_wider(names_from = sig, values_from = n) %>%
  mutate(sum = NO + YES) %>%
  mutate(perc_sig = YES/sum)

#### Air vs. Stream Temperature ####

# Need to first filter only for streamwater temperature records.
temp_data <- chem_data %>%
    # choosing type of measurement
    filter(var %in% c("GN_temp")) %>% # also ran for "IS_temp"
    # and remove interpolated records
    filter(ms_interp == 0) %>%
    select(-c(ms_interp, ms_status, val_err)) %>%
    # and pivot wider
    pivot_wider(names_from = var, values_from = val,
                values_fn = mean) %>%
    rename(watertemp = GN_temp,
           date = datetime)

# And trim down climate data to columns of interest for joining.
clim_trim <- clim %>%
    select(site_code, date, water_year, cc_temp_mean_median) %>%
    rename(airtemp = cc_temp_mean_median)

# Join together only for days on which we have both datasets
airwater_temp <- inner_join(temp_data, clim_trim)
length(unique(airwater_temp$site_code))

(fig_air_water <- ggplot(airwater_temp, aes(x = airtemp, y = watertemp)) +
    geom_point(alpha = 0.8) +
    theme_bw() +
    facet_wrap(.~site_code, scales = "free"))

# ggsave(plot = fig_air_water,
#        filename = "figures/air_vs_water_temperatures_grab.jpg",
#        width = 30,
#        height = 20,
#        units = "cm")

# End of script.
