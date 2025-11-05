# Nitrogen data viz script

#### README ####
# The following script will create sequential
# figures for use in Heili's 2024 AGU talk to
# discuss MacroSheds as a project as well as
# annual & monthly nitrogen trends.

# It will also create figures for Heili's poster
# at the 2025 Gordon Research Conference and
# ultimately the N manuscript.

#### Load packages ####
library(here)
source(here('src', 'setup.R'))

#### Load data ####

# Datasets for AGU figures:
# Annual
# All sites for which we could calculate annual VWM
# n_annual_vwm <- readRDS("data_working/nitrogen_annual_VWM.rds")

# Sites that were actually used in annual-scale trend analyses
# n_annual_vwm_filtered <- readRDS("data_working/nitrogen_annual_VWM_good.rds")

# And annual trends that were calculated.
# n_annual_trends <- readRDS("data_working/nitrogen_annual_trends.rds")

# Including monthly data.
# n_monthly_vwm <- readRDS("data_working/nitrogen_monthly_VWM.rds")

# n_monthly_vwm_filtered <- readRDS("data_working/nitrogen_monthly_VWM_good.rds")

# As well as monthly trends.
# n_monthly_trends <- readRDS("data_working/nitrogen_monthly_trends.rds")

# Datasets for MANUSCRIPT figures:
# Annual
# All sites and analytes for which we could calculate annual VWMs
N_VWM_annual <- readRDS("data_working/N_VWM_annual.rds")

# All sites and analytes for which we could calculate monthly VWMs
N_VWM_monthly <- readRDS("data_working/N_VWM_monthly.rds")

# All sites and analytes for which we could calculate seasonal VWMs
N_VWM_seasonal <- readRDS("data_working/N_VWM_seasonal.rds")

# All sites and analytes for which we could fit annual CQ models.
N_CQ_annual <- readRDS("data_working/N_CQ_annual.rds")

# And seasonal CQ models.
N_CQ_seasonal <- readRDS("data_working/N_CQ_seasonal.rds")

# And seasonal CQ models for the most recent decade.
N_CQ_seasonal20 <- readRDS("data_working/N_CQ_seasonal20.rds")

# And decadal CQ models.
N_CQ_decadal <- readRDS("data_working/N_CQ_decadal.rds")

# NO3 annual trends
no3_trends_ann <- readRDS("data_working/no3_trends_annual.rds")

# NH3 annual trends
nh3_trends_ann <- readRDS("data_working/nh3_trends_annual.rds")

# TDN annual trends
tdn_trends_ann <- readRDS("data_working/tdn_trends_annual.rds")

# N Deposition annual trends
ndep_trends_ann <- readRDS("data_working/ndep_trends_annual.rds")

# Climate trends (from mega_zipper_data.R script)
clim_trends <- read_csv("data_working/trends/full_prisim_climate.csv")

# Discharge metrics
q_metrics <- readRDS("data_working/discharge_metrics_siteyear.rds")

#### AGU Figures ####

##### HB only #####

# This is to illustrate the W6 record at Hubbard Brook, which
# many will be familiar with.
(fig1 <- ggplot(n_annual_vwm %>%
                    # create new column to delineate W6 @ HB
                    mutate(select = factor(case_when(site_code == "w6" ~ "YES",
                                                     TRUE ~ "NO"),
                                           levels = c("YES", "NO"))) %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    color = select)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("black", "transparent")) +
     labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(legend.position = "none",
           axis.title.y = element_markdown(),
           text = element_text(size = 20)))

# ggsave(fig1,
#        filename = "figures/agu_fig1.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### All sites #####

site_count <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code) %>%
    distinct()

# generating additional base year dataset
# so as to remove the connecting lines on the plot
years <- seq(from = 1964, to = 2024, by = 1)
yrs_rep <- rep(years, times = 183)
site_rep <- rep(site_count$site_code, each = 61)
full_site_years <- as.data.frame(cbind(yrs_rep, site_rep))

full_site_years <- full_site_years %>%
    rename(water_year = yrs_rep,
           site_code = site_rep) %>%
    mutate(water_year = as.numeric(water_year)) %>%
    mutate(analyte = "nitrate_N")

# This is the base plot that all others should be built
# around since it includes all possible sites (n = 183).
(fig2 <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing 4 outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL)) +
     geom_line(aes(group = site_code),
               linewidth = 1, color = "black") +
     labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20)))

# ggsave(fig2,
#        filename = "figures/agu_fig2.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### Remove exp. sites #####

n_annual_vwm <- n_annual_vwm %>%
    left_join(., ms_site_data)

site_counts_experimental <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code, ws_status) %>%
    distinct() %>%
    count(ws_status) %>%
    ungroup()

# Color by experimental (n = 36) and
# non-experimental (n = 145) sites.
(fig3 <- ggplot(n_annual_vwm %>%
                    # removes Bonanza Creek sites only
                    drop_na(ws_status) %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    group = site_code,
                    color = ws_status)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("transparent", "black")) +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig3,
#        filename = "figures/agu_fig3.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### Trends #####

# This will include only sites with sufficient
# data for trends (i.e., min. 10 mos per year
# and 10 years within a 20 year timeframe, n = 26).

# First, to keep the same base plot we need to create a
# set of site-wys that passed our filters that we'll use
# to create a new column.
passed <- paste(n_annual_vwm_filtered$site_code,
                n_annual_vwm_filtered$water_year,
                sep = "_")

# Joining this with the site metadata df too to combine
# information about experimental/non-experimental sites
# and those that passed the filters in the same column.
n_annual_vwm <- n_annual_vwm %>%
    mutate(site_code_wy = paste(site_code, water_year, sep = "_")) %>%
    mutate(keep = factor(case_when(site_code_wy %in% passed ~ "YES",
                                    TRUE ~ "NO"),
                         levels = c("YES", "NO"))) %>%
    # also need to combine with experimental watershed filter
    mutate(keepkeep = factor(case_when(keep == "YES" &
                                       ws_status == "non-experimental" ~ "YES",
                                       TRUE ~ "NO"),
                             levels = c("YES", "NO")))

site_counts_sufficientdata <- n_annual_vwm %>%
    select(site_code, keepkeep) %>%
    distinct() %>%
    count(keepkeep) %>%
    ungroup()

(fig4 <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N")%>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    color = keepkeep,
                    group = site_code)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("black", "transparent")) +
     labs(x = "Water Year",
          y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20),
           legend.position = "none"))

# ggsave(fig4,
#        filename = "figures/agu_fig4.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# And now to actually color said trends.
# Pull out trends for NO3 at sites.
sites_trends <- n_annual_trends %>%
    filter(var == "nitrate_N") %>%
    select(site_code, group)

n_annual_vwm <- n_annual_vwm %>%
    left_join(., sites_trends) %>%
    # adding yet another column that combines the filters
    # of interest - passed/exp/sig
    mutate(keepkeepkeep = factor(case_when(keepkeep == "YES" &
                                               group == "sig. increasing" ~ "increasing",
                                           keepkeep == "YES" &
                                               group == "sig. decreasing" ~ "decreasing",
                                           keepkeep == "YES" &
                                               group == "no trend" ~ "no trend",
                                           TRUE ~ "remove"),
                                 levels = c("increasing",
                                            "decreasing",
                                            "no trend",
                                            "remove")))

site_counts_trends <- n_annual_vwm %>%
    select(site_code, keepkeepkeep) %>%
    distinct() %>%
    count(keepkeepkeep) %>%
    ungroup()

(fig5a <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N")%>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                     # add full site-years in to help with
                     # plotting lines properly
                     dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                     "transparent")) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a,
#        filename = "figures/agu_fig5a.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

sen <- function(..., weights = NULL) {
    mblm::mblm(...)
}

# Highlighting positive trends at BES sites.
(fig5a1 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years) %>%
                      mutate(width = case_when(site_code %in% c("BARN",
                                                                "MCDN") ~ "BES",
                                               TRUE ~ "Other")),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      color = keepkeepkeep,
                      linewidth = width)) +
        geom_line() +
        scale_linewidth_manual(values = c(3, 1)) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        # Add increasing trends at BES
        # geom_smooth(data = n_annual_vwm %>%
        # # create additional column to designate which lms show
        # mutate(model = factor(case_when(site_code %in% c("BARN", "MCDN") ~ "YES",
        #                                                 TRUE ~ "NO"),
        #                                       levels = c("YES", "NO"))) %>%
        #                 filter(model == "YES"),
        #             aes(x = water_year,
        #                 y = annual_vwm_mgL,
        #                 group = site_code),
        #             method = sen,
        #             se = F,
        #             color = "#F48849FF",
        #             linewidth = 2) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a1,
#        filename = "figures/agu_fig5a1.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Now, creating zoomed in version of only decreasing sites.
list_keep <- sites_trends %>%
    filter(group == "sig. decreasing")

list_keep <- list_remove$site_code

(fig5a_zoom <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code))) +
        xlim(c(1964, 2024)) +
        geom_line(linewidth = 1, color = "#0D0887FF") +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a_zoom,
#        filename = "figures/agu_fig5a_zoom.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Highlighting negative trends at LUQ sites.
# And removing positive/no trend sites to help with interpretation.
(fig5a2 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                      mutate(coloring = case_when(site_code %in% c("Q1","Q2","Q3") ~ "LUQ",
                                                  TRUE ~ "Other")) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      alpha = coloring,
                      linewidth = coloring)) +
        xlim(c(1964, 2024)) +
        geom_line(color = "#0D0887FF") +
        scale_alpha_manual(values = c(1, 0.2)) +
        scale_linewidth_manual(values = c(3, 1)) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a2,
#        filename = "figures/agu_fig5a2.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Highlighting negative trends at HB sites.
(fig5a3 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                  mutate(coloring = case_when(site_code %in% c("w3","w6","w7", "w8", "w9") ~ "HB",
                                              TRUE ~ "Other")) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      alpha = coloring,
                      linewidth = coloring)) +
        xlim(c(1964, 2024)) +
        geom_line(color = "#0D0887FF") +
        scale_alpha_manual(values = c(1, 0.2)) +
        scale_linewidth_manual(values = c(3, 1)) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a3,
#        filename = "figures/agu_fig5a3.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

#### Monthly #####

site_count_monthly <- n_monthly_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code) %>%
    distinct()

# generating additional base year dataset
# so as to remove the connecting lines on the plot
years <- seq(from = 1964, to = 2024, by = 1)
yrs_rep <- rep(years, times = 183)
site_rep <- rep(site_count_monthly$site_code, each = 61)
full_site_years <- as.data.frame(cbind(yrs_rep, site_rep))

full_site_years <- full_site_years %>%
    rename(year = yrs_rep,
           site_code = site_rep) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(analyte = "nitrate_N")

# Also want to filter out unused data right off the bat.
n_monthly_vwm <- n_monthly_vwm %>%
    left_join(., ms_site_data)

# Create set of site-wys that passed our filters that we'll use
# to create a new column.
passed_m <- paste(n_monthly_vwm_filtered$site_code,
                n_monthly_vwm_filtered$year,
                sep = "_")

# Joining this with the site metadata df too to combine
# information about experimental/non-experimental sites
# and those that passed the filters in the same column.
n_monthly_vwm <- n_monthly_vwm %>%
    mutate(site_code_y = paste(site_code, year, sep = "_")) %>%
    mutate(keep = factor(case_when(site_code_y %in% passed_m ~ "YES",
                                   TRUE ~ "NO"),
                         levels = c("YES", "NO"))) %>%
    # also need to combine with experimental watershed filter
    mutate(keepkeep = factor(case_when(keep == "YES" &
                                           ws_status == "non-experimental" ~ "YES",
                                       TRUE ~ "NO"),
                             levels = c("YES", "NO")))

# Pull out trends for NO3 at sites.
sites_trends_m <- n_monthly_trends %>%
    filter(var == "nitrate_N") %>%
    select(site_code, group)

n_monthly_vwm <- n_monthly_vwm %>%
    left_join(., sites_trends_m) %>%
    # adding yet another column that combines the filters
    # of interest - passed/exp/sig
    mutate(keepkeepkeep = factor(case_when(keepkeep == "YES" &
                                               group == "sig. increasing" ~ "increasing",
                                           keepkeep == "YES" &
                                               group == "sig. decreasing" ~ "decreasing",
                                           keepkeep == "YES" &
                                               group %in% c("ns. increasing",
                                                            "ns. decreasing",
                                                            "no trend")~ "no trend",
                                           TRUE ~ "remove"),
                                 levels = c("increasing",
                                            "decreasing",
                                            "no trend",
                                            "remove")))

# This is the base plot that all others should be built
# around since it includes all possible sites (n = 185).
(fig_monthly <- ggplot(n_monthly_vwm %>%
                     filter(analyte == "nitrate_N")%>%
                     filter(month == 6) %>%
                     # removing outliers for plotting ease
                     filter(monthly_vwm_mgL > 0.000001),
                 aes(x = year,
                     y = monthly_vwm_mgL,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Mean June VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig_monthly,
#        filename = "figures/agu_figmonthly_A.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# And alternatively adding all linear models to the figure
# for those with significant trend.
(fig5b <- ggplot(n_annual_vwm %>%
                     filter(analyte == "nitrate_N")%>%
                     # removing outliers for plotting ease
                     filter(annual_vwm_mgL > 0.000001) %>%
                     filter(annual_vwm_mgL < 40),
                 aes(x = water_year,
                     y = annual_vwm_mgL,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_point(size = 2, shape = 21) +
        scale_fill_manual(values = c("#55C667FF",
                                     "#404788FF",
                                     "transparent",
                                     "transparent")) +
        geom_smooth(data = n_annual_vwm %>%
                        filter(keepkeepkeep %in% c("increasing",
                                                   "decreasing")),
                    aes(x = water_year,
                        y = annual_vwm_mgL,
                        group = site_code,
                        fill = keepkeepkeep,
                        color = keepkeepkeep),
                    method = "lm",
                    se = F) +
        scale_color_manual(values = c("#55C667FF",
                                      "#404788FF",
                                      "grey70",
                                      "transparent")) +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5b,
#        filename = "figures/agu_fig5b.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### HB Specific #####

# Creating a historical monthly dataset for HB data (1965-1975)
(fig_hb_past <- ggplot(n_monthly_vwm %>%
                           filter(analyte == "nitrate_N") %>%
                           filter(site_code == "w6") %>%
                           filter(year >= 1965) %>%
                           filter(year < 1975),
                       aes(x = factor(month),
                           y = monthly_vwm_mgL)) +
        geom_boxplot(fill = "#0D0887FF",
                     color = "#0D0887FF",
                     alpha = 0.2) +
        ylim(0, 1.25) +
        labs(x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20)))

(fig_hb_present <- ggplot(n_monthly_vwm %>%
                           filter(analyte == "nitrate_N") %>%
                           filter(site_code == "w6") %>%
                           filter(year >= 2010) %>%
                           filter(year < 2020),
                       aes(x = factor(month),
                           y = monthly_vwm_mgL)) +
        geom_boxplot(fill = "#0D0887FF",
                     color = "#0D0887FF",
                     alpha = 0.2) +
        ylim(0, 1.25) +
        labs(x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20)))

(fig_hb <- fig_hb_past + fig_hb_present)

# ggsave(fig_hb,
#        filename = "figures/agu_figHB.jpeg",
#        height = 16,
#        width = 40,
#        units = "cm")

#### Drivers ####

##### Temperature #####

# Load in climate data.
clim_raw <- read_feather(here('data_raw',
                          'spatial_timeseries_climate.feather'))
clim <- clim_raw %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date))

# Create new columns to add to 41 sites for plotting.
sites_to_plot <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    filter(keepkeepkeep %in% c("decreasing",
                              "increasing",
                              "no trend")) %>%
    select(site_code, keepkeepkeep) %>%
    unique()

my41sites <- sites_to_plot$site_code

# Filter climate dataset for sites of interest
clim_trim_annual <- clim %>%
    filter(site_code %in% my41sites) %>%
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp = mean(temp_median, na.rm = TRUE),
              sum_N_dep = sum(N_flux_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    full_join(sites_to_plot)

(fig6 <- ggplot(clim_trim_annual %>%
                    # filter out strange final year
                    filter(water_year < 2023) %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                 aes(x = water_year,
                     y = mean_ann_temp,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Mean Annual Temperature (C)") +
        theme_bw() +
        facet_grid(keepkeepkeep~.) +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none",
              strip.background = element_blank(),
              strip.text.y = element_blank()))

# ggsave(fig6,
#        filename = "figures/agu_fig6.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

##### Deposition #####

# Need to make new dataset for deposition since it's
# already annualized.
clim_dep <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% my41sites) %>%
    full_join(sites_to_plot)

(fig7 <- ggplot(clim_dep %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                aes(x = year,
                    y = N_flux_mean,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("#F48849FF",
                                   "#0D0887FF",
                                   "grey80",
                                   "transparent")) +
     labs(x = "Year",
          y = "Cumulative Annual N Deposition (kg/ha)") +
     theme_bw() +
     facet_grid(keepkeepkeep~.) +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20),
           legend.position = "none",
           strip.background = element_blank(),
           strip.text.y = element_blank()))

# ggsave(fig7,
#        filename = "figures/agu_fig7.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

##### Productivity #####

prod_raw <- read_feather(here('data_raw',
                              'spatial_timeseries_vegetation.feather'))

# Filter productivity dataset for sites of interest
prod <- prod_raw %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date))

prod_trim_annual <- prod %>%
    filter(site_code %in% my41sites) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup() %>%
    full_join(sites_to_plot)

(fig8 <- ggplot(prod_trim_annual %>%
                    # remove weird final years
                    filter(water_year < 2021) %>%
                    # and need to remove LUQ sites
                    # where we don't have GPP
                    filter(sum_ann_prod > 0) %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                aes(x = water_year,
                    y = sum_ann_prod,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Cumulative Annual GPP (kg C/m<sup>2</sup>)") +
        theme_bw() +
        facet_grid(keepkeepkeep~.) +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none",
              strip.background = element_blank(),
              strip.text.y = element_blank()))

# ggsave(fig8,
#        filename = "figures/agu_fig8.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

#### Manuscript Figures ####

##### Data Coverage #####

# Below, we need to figure out a way to display the
# relative coverage of the different analytes, their
# annual VWM concentrations, and the relative certainty
# of these estimates (i.e., no. of observations per site).

# Calculate means of VWM concentrations & number of obs.
mean_N_VWM_annual <- N_VWM_annual %>%
    group_by(site_code, analyte_N) %>%
    summarize(mean_annual_VWM_mgL = mean(annual_vwm_mgL,
                                           na.rm = TRUE),
              mean_annual_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Set color legend break points.
my_breaks <- c(1, 10, 50)

# This is an initial version of summary figure in the manuscript.
(summaryfig1 <- ggplot(mean_N_VWM_annual,
                      aes(x = mean_annual_VWM_mgL,
                          y = analyte_N,
                          fill = mean_annual_obs,
                          size = total_years)) +
    geom_jitter(width = 0.05, alpha = 0.9, shape = 21) +
    scale_fill_gradientn(colors = c("white", "#FAB455",
                                    "#69B9FA", "#59A3F8",
                                    "#4B9FF7", "#045CB4", "black"),
                          trans = "log",
                         breaks = my_breaks, labels = my_breaks) +
    scale_size_continuous(breaks = my_breaks) +
    scale_x_log10() +
    facet_grid(analyte_N~., scales = "free") +
    labs(x = "Mean Annual Concentration (mg/L)",
         y = "Analyte",
         fill = "Mean No. of Annual Observations",
         size = "Record Length (yrs)") +
    theme_bw() +
    theme(strip.text.y = element_blank(),
          legend.position = "top"))

# ggsave(summaryfig1,
#        filename = "figures/summaryfig_allN.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

# Also, wanted to create a data coverage figure for reference.
site_counts <- mean_N_VWM_annual %>%
    count(analyte_N)

(summaryfig2 <- ggplot(N_VWM_annual,
                       aes(x = water_year,
                           y = site_code)) +
        geom_line() +
        facet_grid(.~analyte_N, scales = "free") +
        labs(x = "Year",
             y = "Site") +
        theme_bw() +
        theme(axis.text.y = element_blank()))

# This figure gives a better idea as to data density
# within a given time frame.
ts_counts <- N_VWM_annual %>%
    count(analyte_N, water_year)

(summaryfig3 <- ggplot(ts_counts,
                       aes(x = water_year,
                           y = n)) +
        geom_line() +
        facet_grid(.~analyte_N) +
        labs(x = "Year",
             y = "Site No.") +
        theme_bw())

# ggsave(summaryfig3,
#        filename = "figures/summaryfig_ts_sitecounts.jpeg",
#        height = 10,
#        width = 30,
#        units = "cm")

# Calculate means of VWM concentrations & number of obs.
# BUT FILTER DOWN TO 2010-2020 [INCLUSIVE]
# AND REMOVE EXPERIMENTAL SITES
# for better comparison in the summary figure.
mean_N_VWM_annual20 <- N_VWM_annual %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, analyte_N) %>%
    summarize(mean_annual_VWM_mgL = mean(annual_vwm_mgL,
                                           na.rm = TRUE),
              mean_annual_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Join with site info so that we can filter out experimental sites.
mean_N_VWM_annual20 <- left_join(mean_N_VWM_annual20, ms_site_data)

mean_N_VWM_annual20_nonexp <- mean_N_VWM_annual20 %>%
    filter(ws_status == "non-experimental")

# Set color legend break points.
color_breaks <- c(1, 10, 50)
size_breaks <- c(1,5,10)

# Count number of sites per analyte
site_counts20 <- mean_N_VWM_annual20_nonexp %>%
    count(analyte_N)

# And create list with which to add in count annotations
dat_text <- data.frame(
    label = c("n = 116", "n = 20", "n = 116", "n = 46", "n = 142",
              "n = 84", "n = 10", "n = 1", "n = 60", "n = 31"),
    analyte_N = c("DIN", "N2O", "NH3_N", "NO2_N", "NO3_N",
                  "TDN", "TIN", "TKN", "TN", "TPN"))

# This is the figure included in the manuscript as the
# first data summary figure.

# Note, the log transformation on the x-axis only removes
# one site's data for one analyte - ONO2 for NH3_N.
(summaryfig4 <- ggplot(mean_N_VWM_annual20_nonexp) +
        geom_jitter(aes(x = mean_annual_VWM_mgL,
                        y = analyte_N,
                        fill = mean_annual_obs,
                        size = total_years),
                    width = 0.05, alpha = 0.9, shape = 21) +
        scale_fill_gradientn(colors = c("white", "#FAB455",
                                        "#69B9FA", "#59A3F8",
                                        "#4B9FF7", "#045CB4", "black"),
                             trans = "log",
                             breaks = color_breaks,
                             labels = color_breaks) +
        scale_size_continuous(breaks = size_breaks) +
        scale_x_log10(labels = scales::comma,
                      breaks = c(0.001, 0.01, 0.1, 1, 10)) +
        facet_grid(analyte_N~., scales = "free") +
        labs(x = "Mean Annual Concentration (mg/L)",
             y = "Analyte",
             fill = "Mean No. of Annual Observations",
             size = "Record Length (yrs)") +
        geom_text(data = dat_text,
                  mapping = aes(x = 20, y = analyte_N, label = label)) +
        theme_bw() +
        theme(strip.text.y = element_blank(),
              legend.position = "top"))

# ggsave(summaryfig4,
#        filename = "figures/summaryfig_N_2010_to_2020.jpeg",
#        height = 20,
#        width = 24,
#        units = "cm")

# Now, I also want to create a summary figure of sorts for
# seasonal data, so will do so using a similar style as above.

# Calculate means of VWM concentrations & number of obs.
# BUT FILTER DOWN TO 2010-2020 [INCLUSIVE]
# AND REMOVE EXPERIMENTAL SITES
# for better comparison in the summary figure.
mean_N_VWM_seasonal20 <- N_VWM_seasonal %>%
    filter(season_year > 2009) %>%
    filter(season_year < 2021) %>%
    group_by(site_code, season, analyte_N) %>%
    summarize(mean_seasonal_VWM_mgL = mean(seasonal_vwm_mgL,
                                         na.rm = TRUE),
              mean_seasonal_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup() %>%
    mutate(season = factor(season, levels = c("Spring",
                                              "Summer",
                                              "Fall",
                                              "Winter")))

# Join with site info so that we can filter out experimental sites.
mean_N_VWM_seasonal20 <- left_join(mean_N_VWM_seasonal20, ms_site_data)

mean_N_VWM_seasonal20_nonexp <- mean_N_VWM_seasonal20 %>%
    filter(ws_status == "non-experimental")

# Test figure.
# Set color legend break points.
color_breaks <- c(1, 3, 12)
size_breaks <- c(1,5,10)

# Count number of sites per analyte
site_counts20_seas <- mean_N_VWM_seasonal20_nonexp %>%
    count(analyte_N, season)

# And create list with which to add in count annotations
dat_text_seas <- data.frame(
    position = c(10, 10, 10, 10,
                 0.0002, 0.0002, 0.0002, 0.0002,
                 0.0002, 0.0002, 0.0002, 0.0002),
    label = c("n = 87", "n = 85", "n = 97", "n = 90",
              "n = 113", "n = 115", "n = 129", "n = 119",
              "n = 73", "n = 73", "n = 82", "n = 64"),
    season = c("Spring", "Summer", "Fall", "Winter",
               "Spring", "Summer", "Fall", "Winter",
               "Spring", "Summer", "Fall", "Winter"),
    analyte_N = c("NH3_N", "NH3_N", "NH3_N", "NH3_N",
                  "NO3_N", "NO3_N", "NO3_N", "NO3_N",
                  "TDN", "TDN", "TDN", "TDN"))

# Note, the log transformation on the x-axis only removes
# one site's data for one analyte - ONO2 for NH3_N.
(summaryfig5 <- ggplot(mean_N_VWM_seasonal20_nonexp %>%
                           filter(analyte_N %in% c("NO3_N",
                                                   "NH3_N",
                                                   "TDN")) %>%
                           # remove 3 outliers that caused axis stretch
                           filter(mean_seasonal_VWM_mgL > 0.0000001)) +
        geom_jitter(aes(x = mean_seasonal_VWM_mgL,
                        y = season,
                        fill = mean_seasonal_obs,
                        size = total_years),
                    height = 0.1, alpha = 0.9, shape = 21) +
        scale_fill_gradientn(colors = c("white", "#FAB455",
                                        "#69B9FA", #"#59A3F8",
                                        "#4B9FF7", "#045CB4", "black"),
                             trans = "log",
                             breaks = color_breaks,
                             labels = color_breaks) +
        scale_size_continuous(breaks = size_breaks) +
        scale_x_log10(labels = scales::comma,
                      breaks = c(0.001, 0.01, 0.1, 1, 10)) +
        facet_grid(analyte_N~., scales = "free") +
        labs(x = "Mean Seasonal Concentration (mg/L)",
             y = "Season",
             fill = "Mean No. of Seasonal Observations",
             size = "Record Length (yrs)") +
        geom_text(data = dat_text_seas,
                  mapping = aes(x = position, y = season, label = label)) +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "top"))

# ggsave(summaryfig5,
#        filename = "figures/summaryfig_N_season_2010_to_2020.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

##### Peak Months #####

# Using a similar workflow as above, I am going to create a paneled
# figure showing peak months for Q and N analytes.

# First, I need to calculate monthly averages
mean_N_VWM_monthly20 <- N_VWM_monthly %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, analyte_N, month) %>%
    summarize(mean_monthly_VWM_mgL = mean(monthly_vwm_mgL,
                                         na.rm = TRUE),
              mean_monthly_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Join with site info so that we can filter out experimental sites.
mean_N_VWM_monthly20 <- left_join(mean_N_VWM_monthly20, ms_site_data)

mean_N_VWM_monthly20_nonexp <- mean_N_VWM_monthly20 %>%
    filter(ws_status == "non-experimental")

# Also, to impose at least a minimal filter on these data I am going to
# make a list of sites with a minimum of 3 months of data for 6 years in
# this time frame.

# Taking the average of months represented and years in records.
mean_times <- mean_N_VWM_monthly20_nonexp %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    group_by(site_code, analyte_N) %>%
    summarize(total_months = n(),
              avg_years = mean(total_years)) %>%
    ungroup()

keep_sites <- mean_times %>%
    mutate(keep = case_when(total_months > 2 &
                                avg_years >= 6 ~ "YES",
                            TRUE ~ "NO")) %>%
    filter(keep == "YES") %>%
    select(site_code, analyte_N)

# And trim down original monthly VWM data to match the above list.
mean_N_VWM_monthly20_nonexp_keep <- left_join(keep_sites,
                                              mean_N_VWM_monthly20_nonexp)

# Also, need to bring in Q data to do the same.
q_monthly <- q_metrics %>%
    filter(agg_code %in% c("1", "2", "3", "4", "5", "6",
                           "7", "8", "9", "10", "11", "12"))

# calculate monthly averages
# First, I need to calculate monthly averages
mean_q_monthly20 <- q_monthly %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, month) %>%
    summarize(mean_monthly_q = mean(q_mean, na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup() %>%
    filter(total_years > 6) %>%
    filter(!is.nan(mean_monthly_q)) %>%
    filter(!is.na(month))

# And find peak discharge months.
peak_q_months <- mean_q_monthly20 %>%
    group_by(site_code) %>%
    slice_max(mean_monthly_q) %>%
    mutate(analyte_N = "Q")

# Calculate peak months.
peak_months <- mean_N_VWM_monthly20_nonexp_keep %>%
    group_by(site_code, analyte_N) %>%
    slice_max(mean_monthly_VWM_mgL) %>%
    # and join with Q data above
    full_join(peak_q_months) %>%
    # reorder analytes %>%
    mutate(analyte = factor(analyte_N,
                            levels = c("Q",
                                       "NO3_N",
                                       "NH3_N",
                                       "TDN"))) %>%
    ungroup()

# Peak months figure
(q_peaks <- ggplot(peak_months %>%
                       filter(analyte == "Q"), aes(x = month)) +
    geom_bar(stat = "count", fill = "#4B8FF7") +
    labs(x = "Month", y = "Count", title = "Q") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       limits = c(1,12)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)))

(no3_peaks <- ggplot(peak_months %>%
                       filter(analyte == "NO3_N"), aes(x = month)) +
        geom_bar(stat = "count", fill = "#E7A655") +
        labs(x = "Month", y = "Count", title = "Nitrate") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(1,12)) +
        scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),
                           limits = c(0,10)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)))

(nh3_peaks <- ggplot(peak_months %>%
                         filter(analyte == "NH3_N"), aes(x = month)) +
        geom_bar(stat = "count", fill = "#E38377") +
        labs(x = "Month", y = "Count", title = "Ammonium") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(1,12)) +
        scale_y_continuous(breaks = c(1,2,3,4,5,6),
                           limits = c(0,6)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)))

(tdn_peaks <- ggplot(peak_months %>%
                         filter(analyte == "TDN"), aes(x = month)) +
        geom_bar(stat = "count", fill = "#6D4847") +
        labs(x = "Month", y = "Count", title = "Total Dissolved Nitrogen") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(1,12)) +
        scale_y_continuous(breaks = c(1,2,3,4,5,6),
                           limits = c(0,6)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)))

# And combine them into a single figure.
(peaks_all <- (q_peaks / no3_peaks / nh3_peaks / tdn_peaks))

# And export figure.
# ggsave(peaks_all,
#        filename = "figures/peak_months_N_2010_to_2020.jpeg",
#        height = 20,
#        width = 10,
#        units = "cm")

##### Site Attributes #####

# Also going to make plots to investigate watershed and climate
# characteristics with magnitude of nitrogen at each site.
# Focusing in on 2010-2020 data to keep things comparable.

# First need to make a guiding site list.
# List of sites included in the 2010-2020 dataset
annual_sites20 <- unique(mean_N_VWM_annual20_nonexp$site_code)

mean_N_VWM_annual <- left_join(mean_N_VWM_annual, ms_site_data)

mean_N_VWM_annual_nonexp <- mean_N_VWM_annual %>%
    filter(ws_status == "non-experimental")

# And a list of sites included in the *full* dataset
annual_sites <- unique(mean_N_VWM_annual_nonexp$site_code)

all_sites <- as.data.frame(annual_sites) %>%
    mutate(timeframe = case_when(annual_sites %in% annual_sites20 ~ "recent",
                                 TRUE ~ "full"))

# Load in raw climate data.
clim_raw <- read_feather(here('data_raw',
                              'ms',
                              'v2',
                              'spatial_timeseries_climate.feather'))
# This takes a bit, so be patient.

clim <- clim_raw %>%
    # adds water year column
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    # trims columns of interest
    select(site_code, date, water_year, var, val) %>%
    # pivots and aggregates daily data
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    # adds month column
    mutate(month = month(date))

# Aggregate climate data annually.
# Full dataset
clim_annual <- clim %>%
    # select only for sites of interest
    filter(site_code %in% annual_sites) %>%
    # calculate annual metrics
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp = mean(temp_median, na.rm = TRUE),
              sum_ann_ppt = sum(precip_median, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
clim_annual20 <- clim %>%
    # select only for sites of interest
    filter(site_code %in% annual_sites20) %>%
    # select for only timeframe of interest (2010-2020)
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    # calculate annual metrics
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp20 = mean(temp_median, na.rm = TRUE),
              sum_ann_ppt20 = sum(precip_median, na.rm = TRUE)) %>%
    ungroup()

# Further aggregates climate data to site-level.
# Full dataset
clim_decadal <- clim_annual %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_temp = mean(mean_ann_temp, na.rm = TRUE),
              mean_sum_ann_ppt = mean(sum_ann_ppt, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
clim_decadal20 <- clim_annual20 %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_temp20 = mean(mean_ann_temp20, na.rm = TRUE),
              mean_sum_ann_ppt20 = mean(sum_ann_ppt20, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_clim_data <- full_join(mean_N_VWM_annual_nonexp, clim_decadal)
N_clim_data20 <- full_join(mean_N_VWM_annual20_nonexp, clim_decadal20)

# N deposition
# New dataset since deposition is already annualized
# Full dataset
clim_deponly <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% annual_sites)

# Only 2010-2020
clim_deponly20 <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% annual_sites20) %>%
    filter(year > 2009) %>%
    filter(year < 2021)

# Further aggregates deposition data at site-level.
# Full dataset
dep_decadal <- clim_deponly %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_Ndep = mean(N_flux_mean, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
dep_decadal20 <- clim_deponly20 %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_Ndep20 = mean(N_flux_mean, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_dep_data <- full_join(mean_N_VWM_annual_nonexp, dep_decadal)
N_dep_data20 <- full_join(mean_N_VWM_annual20_nonexp, dep_decadal20)

# Adding GPP to the list of available WS attributes
prod_raw <- read_feather(here('data_raw',
                              'ms',
                              'v2',
                              'spatial_timeseries_vegetation.feather'))

# Filter productivity dataset for sites of interest
prod <- prod_raw %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date))

# Annually aggregate productivity data.
# Full dataset
prod_annual <- prod %>%
    filter(site_code %in% annual_sites) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
prod_annual20 <- prod %>%
    filter(site_code %in% annual_sites20) %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod20 = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup()

# Further aggregates productivity data at the site-level
# Full dataset
prod_decadal <- prod_annual %>%
    group_by(site_code) %>%
    summarize(mean_sum_ann_prod = mean(sum_ann_prod, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
prod_decadal20 <- prod_annual20 %>%
    group_by(site_code) %>%
    summarize(mean_sum_ann_prod20 = mean(sum_ann_prod20, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_gpp_data <- full_join(mean_N_VWM_annual, prod_decadal)
N_gpp_data20 <- full_join(mean_N_VWM_annual20, prod_decadal20)

# Overall WS attributes

# Calculated total wetland cover.
ms_ws_attr <- ms_ws_attr %>%
    mutate(nlcd_wetland = nlcd_water + nlcd_wetland_herb + nlcd_wetland_wood,
           nlcd_dev = nlcd_dev_hi + nlcd_dev_med + nlcd_dev_low + nlcd_dev_open)

ms_ws_select <- ms_ws_attr %>%
    select(network, domain, site_code, area, slope_mean, elev_mean,
           nlcd_dev, nlcd_wetland)

# Realizing it likely makes more sense to plot overall distribution
# of attributes rather than by analyte, so going to work on that below.

# Aggregate all datasets together.
# Need to start with the most complete dataset - ws attributes.
N_data <- ms_ws_select %>% filter(site_code %in% annual_sites)
N_data <- left_join(N_data, dep_decadal)
N_data <- left_join(N_data, prod_decadal)
N_data <- left_join(N_data, clim_decadal)

N_data20 <- ms_ws_select %>% filter(site_code %in% annual_sites20)
N_data20 <- left_join(N_data20, dep_decadal20)
N_data20 <- left_join(N_data20, prod_decadal20)
N_data20 <- left_join(N_data20, clim_decadal20)

(deppanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = mean_mean_ann_Ndep),
                     color = "NA",
                     fill = "#E29244", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = mean_mean_ann_Ndep20),
                     color = "#E29244", linewidth = 2) +
        labs(x = "Cumulative Annual N Deposition (kg/ha)",
             y = "Density") +
        theme_bw())

(temppanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = mean_mean_ann_temp),
                     color = "NA",
                     fill = "#D46F10", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = mean_mean_ann_temp20),
                     color = "#D46F10", linewidth = 2) +
        labs(x = "Mean Annual Temperature (C)") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(gpppanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = mean_sum_ann_prod),
                     color = "NA",
                     fill = "#4CA49E", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = mean_sum_ann_prod20),
                     color = "#4CA49E", linewidth = 2) +
        labs(x = "Cumulative Annual GPP (kg C/m<sup>2</sup>)",
             y = "Density") +
        theme_bw() +
        theme(axis.title.x = element_markdown()))

(pptpanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = mean_sum_ann_ppt),
                     color = "NA",
                     fill =  "#69B9FA", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = mean_sum_ann_ppt20),
                     color = "#69B9FA", linewidth = 2) +
        labs(x = "Cumulative Annual Precipitation (mm)") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(sizepanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = area),
                     color = "NA",
                     fill =  "#59A3F8", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = area),
                     color = "#59A3F8", linewidth = 2) +
        scale_x_log10() +
        labs(x = "Watershed Area (ha)",
             y = "Density") +
        theme_bw())

(slopepanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = slope_mean),
                     color = "NA",
                     fill =  "#4B8FF7", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = slope_mean),
                     color = "#4B8FF7", linewidth = 2) +
        labs(x = "Watershed Slope (degrees)") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(elevpanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = elev_mean),
                     na.rm = TRUE,
                     color = "NA",
                     fill = "#5A7ECB", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = elev_mean),
                     color = "#5A7ECB", linewidth = 2) +
        labs(x = "Watershed Elevation (m)") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(wetlandpanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = nlcd_wetland),
                     color = "NA",
                     fill =  "#6B6D9F", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = nlcd_wetland),
                     color = "#6B6D9F", linewidth = 2) +
        scale_x_log10() +
        labs(x = "% Wetland Land Cover") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(devpanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = nlcd_dev),
                     color = "NA",
                     fill =  "#1E2F46", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = nlcd_dev),
                     color = "#1E2F46", linewidth = 2) +
        scale_x_log10() +
        labs(x = "% Developed Land Cover") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(ws_attributes <- (deppanel + temppanel + pptpanel)/
        (gpppanel + wetlandpanel + devpanel)/
        (sizepanel + slopepanel + elevpanel))

# ggsave(ws_attributes,
#        filename = "figures/summaryfig_attributes_dens_hist_v_recent.jpeg",
#        height = 16,
#        width = 24,
#        units = "cm")

# Making a larger correlation plot that is more inclusive of
# site-level attributes and comparing to annual N concentrations.

# First, need to create the base dataset using 2010-2020 data only
# and excluding experimental sites.
# Combine N dataset with calculated attributes & climate indices.
N_corr_dat <- left_join(mean_N_VWM_annual20_nonexp, ms_ws_attr)
N_corr_dat <- left_join(N_corr_dat, dep_decadal20)
N_corr_dat <- left_join(N_corr_dat, prod_decadal20)
N_corr_dat <- left_join(N_corr_dat, clim_decadal20)

# And we'll trim this down to the three most common analytes
# as well as the columns of interest.
N_corr_dat_trim <- N_corr_dat %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    select(site_code, analyte_N, mean_annual_VWM_mgL, mean_annual_obs,
           latitude, longitude, ws_area_ha, slope_mean, elev_mean,
           aspect_mean, nlcd_wetland, nlcd_dev, mean_mean_ann_Ndep20,
           mean_sum_ann_prod20, mean_mean_ann_temp20, mean_sum_ann_ppt20,
           first_record, last_record)

# And then pivot it for easy plot creation.
N_corr_dat_trim_long <- N_corr_dat_trim %>%
    select(-first_record,-last_record) %>%
    pivot_longer(cols = mean_annual_obs:mean_sum_ann_ppt20,
                 names_to = "var", values_to = "val")

# Now, to make a plot.
(corr <- ggplot(N_corr_dat_trim_long,
                 aes(x = val,
                     y = mean_annual_VWM_mgL)) +
        geom_point() +
        labs(y = "VWM (mg/L)") +
        facet_grid(var~analyte_N, scales = "free") +
        theme_bw()) # well, that didn't work, so individual plots it is...

# First, I'm going to calculate correlations between all of these
# data.
corr_values <- N_corr_dat_trim %>%
    group_by(analyte_N) %>%
    summarize(corr_obs = cor(mean_annual_obs, mean_annual_VWM_mgL),
              corr_lat = cor(latitude, mean_annual_VWM_mgL),
              corr_lon = cor(longitude, mean_annual_VWM_mgL),
              corr_area = cor(ws_area_ha, mean_annual_VWM_mgL,
                              use = "pairwise.complete.obs"),
              corr_slope = cor(slope_mean, mean_annual_VWM_mgL,
                               use = "pairwise.complete.obs"),
              corr_elev = cor(elev_mean, mean_annual_VWM_mgL,
                              use = "pairwise.complete.obs"),
              corr_aspect = cor(aspect_mean, mean_annual_VWM_mgL,
                                use = "pairwise.complete.obs"),
              corr_wet = cor(nlcd_wetland, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs"),
              corr_dev = cor(nlcd_dev, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs"),
              corr_temp = cor(mean_mean_ann_temp20, mean_annual_VWM_mgL,
                              use = "pairwise.complete.obs"),
              corr_ppt = cor(mean_sum_ann_ppt20, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs"),
              corr_gpp = cor(mean_sum_ann_prod20, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs"),
              corr_dep = cor(mean_mean_ann_Ndep20, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs")) %>%
    ungroup()
# Will include corr values on plots below if >0.3.

# Building individual plots to combine later.
(corr1 <- ggplot(N_corr_dat_trim,
                 aes(x = first_record,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Date of 1st Record", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw())

(corr2 <- ggplot(N_corr_dat_trim,
                 aes(x = last_record,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Date of Last Record", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank()))

(corr3 <- ggplot(N_corr_dat_trim,
                 aes(x = mean_annual_obs,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Mean Annual Obs.", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank()))

(corr4 <- ggplot(N_corr_dat_trim,
                 aes(x = latitude,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Latitude", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank()))

(corr5 <- ggplot(N_corr_dat_trim,
                 aes(x = longitude,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Longitude", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank()))

(corr6 <- ggplot(N_corr_dat_trim,
                 aes(x = ws_area_ha,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        scale_x_log10() +
        labs(x = "Area (ha)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(axis.title.y = element_blank()))

(corr7 <- ggplot(N_corr_dat_trim,
                 aes(x = slope_mean,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Mean Slope", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

(corr8 <- ggplot(N_corr_dat_trim,
                 aes(x = elev_mean,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Mean Elevation", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

(corr9 <- ggplot(N_corr_dat_trim,
                 aes(x = aspect_mean,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Mean Aspect", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

(corr10 <- ggplot(N_corr_dat_trim,
                 aes(x = nlcd_wetland,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "% Wetland", y = "VWM (mg/L)") +
        scale_x_log10() +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

# List with which to add in correlation annotations
dat_corr_dev <- data.frame(
    xposition = c(25, 10, 10),
    yposition = c(0.18, 5, 5),
    label = c("0.33", " ", " "),
    analyte_N = c("NH3_N", "NO3_N", "TDN"))

(corr11 <- ggplot(N_corr_dat_trim,
                  aes(x = nlcd_dev,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_dev,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label),
                  color = "red", fontface = "bold") +
        labs(x = "% Developed", y = "VWM (mg/L)") +
        scale_x_log10() +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(axis.title.y = element_blank()))

dat_corr_temp <- data.frame(
    xposition = c(15, 12.5, 12.5),
    yposition = c(0.175, 5, 4.75),
    label = c(" ", "0.32", "0.41"),
    analyte_N = c("NH3_N", "NO3_N", "TDN"))

(corr12 <- ggplot(N_corr_dat_trim,
                  aes(x = mean_mean_ann_temp20,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_temp,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label),
                  color = "red", fontface = "bold") +
        labs(x = "Mean Annual Temp. (°C)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

dat_corr_ppt <- data.frame(
    xposition = c(2000, 2000, 2000),
    yposition = c(0.175, 5, 4.75),
    label = c(" ", " ", "-0.42"),
    analyte_N = c("NH3_N", "NO3_N", "TDN"))

(corr13 <- ggplot(N_corr_dat_trim,
                  aes(x = mean_sum_ann_ppt20,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_ppt,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label),
                  color = "blue", fontface = "bold") +
        labs(x = "Mean Annual Ppt. (mm)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        xlim(0, 3000) +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

dat_corr_gpp <- data.frame(
    xposition = c(1.75, 1.75, 1.75),
    yposition = c(0.18, 5, 4.75),
    label = c("0.34", " ", " "),
    analyte_N = c("NH3_N", "NO3_N", "TDN"))

(corr14 <- ggplot(N_corr_dat_trim,
                  aes(x = mean_sum_ann_prod20,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_gpp,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label),
                  color = "red", fontface = "bold") +
        labs(x = "Mean Annual Prod. (kg C/m^2)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

dat_corr_dep <- data.frame(
    xposition = c(5, 5, 5),
    yposition = c(0.18, 5, 4.75),
    label = c("0.34", " ", "-0.40"),
    analyte_N = c("NH3_N", "NO3_N", "TDN"),
    sign = c("positive", NA, "negative"))

(corr15 <- ggplot(N_corr_dat_trim,
                  aes(x = mean_mean_ann_Ndep20,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_dep,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label,
                                color = sign),
                  fontface = "bold") +
        scale_color_manual(values = c("blue", "red")) +
        labs(x = "Mean Annual N Dep. (kg/ha)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none"))

# And combine them into a single figure.
(corr_all <- ((corr1 / corr2 / corr3 / corr4 / corr5) |
                 (corr6 / corr7 / corr8 / corr9 / corr10) |
                    (corr11 / corr12 / corr13 / corr14 / corr15)))

# And export figure.
# ggsave(corr_all,
#        filename = "figures/correlationfig_N_2010_to_2020.jpeg",
#        height = 25,
#        width = 40,
#        units = "cm")

##### CQ #####

###### Seasonal #######

# Filter down to desired analytes and minimum of 10 observations,
# using data from the past 10 years.
N_CQ_trim <- N_CQ_seasonal20 %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    filter(n_of_obs > 9)

# And join with site data to filter out experimental sites.
N_CQ_trim <- left_join(N_CQ_trim, ms_site_data)

N_CQ_trim_nonexp <- N_CQ_trim %>%
    filter(ws_status == "non-experimental")

(n_cq_seas <- ggplot(N_CQ_trim_nonexp) +
        geom_histogram(aes(x = cq_slope,
                           fill = ..x..),
                       color = "black", bins = 30) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_fill_gradient2(low = "#045CB4",
                             mid = "white",
                             high = "#FAB455",
                             midpoint = 0) +
        facet_grid(analyte_N~season, scales = "free") +
        labs(x = "Seasonal C-Q Slope", y = "Site Count") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none"))

# ggsave(n_cq_seas,
#        filename = "figures/seasonal_cq_slope_2010_to_2020.jpeg",
#        height = 16,
#        width = 16,
#        units = "cm")

(n_int_seas <- ggplot(N_CQ_trim_nonexp) +
        geom_histogram(aes(x = cq_int,
                           fill = ..x..),
                       color = "black", bins = 30) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_fill_gradient2(low = "#045CB4",
                             mid = "white",
                             high = "#FAB455",
                             midpoint = 0) +
        facet_grid(analyte_N~season, scales = "free") +
        labs(x = "Seasonal C-Q Intercept", y = "Site Count") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none"))

# ggsave(n_int_seas,
#        filename = "figures/seasonal_cq_int_2010_to_2020.jpeg",
#        height = 16,
#        width = 16,
#        units = "cm")

###### Decadal #######

# Filter down to desired analytes and minimum of 10 observations.
N_CQ_dec_trim <- N_CQ_decadal %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    filter(n_of_obs > 9)

# And join with site data to filter out experimental sites.
N_CQ_dec_trim <- left_join(N_CQ_dec_trim, ms_site_data)

N_CQ_dec_trim_nonexp <- N_CQ_dec_trim %>%
    filter(ws_status == "non-experimental")

(n_cq_dec <- ggplot(N_CQ_dec_trim_nonexp) +
        geom_histogram(aes(x = cq_slope,
                           fill = ..x..),
                       color = "black", bins = 30) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        # this omits an outlier, but doing this for consistent viewing
        xlim(-2, 2) +
        scale_fill_gradient2(low = "#045CB4",
                             mid = "white",
                             high = "#FAB455",
                             midpoint = 0) +
        facet_grid(analyte_N~decade, scales = "free") +
        labs(x = "Decadal C-Q Slope", y = "Site Count") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none"))

# ggsave(n_cq_dec,
#        filename = "figures/decadal_cq_slope.jpeg",
#        height = 16,
#        width = 32,
#        units = "cm")

(n_int_dec <- ggplot(N_CQ_dec_trim_nonexp) +
        geom_histogram(aes(x = cq_int,
                           fill = ..x..),
                       color = "black", bins = 30) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        # this omits an outlier, but doing this for consistent viewing
        xlim(-15, 5) +
        scale_fill_gradient2(low = "#045CB4",
                             mid = "white",
                             high = "#FAB455",
                             midpoint = 0) +
        facet_grid(analyte_N~decade, scales = "free") +
        labs(x = "Decadal C-Q Intercept", y = "Site Count") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none"))

# ggsave(n_int_dec,
#        filename = "figures/decadal_cq_intercept.jpeg",
#        height = 16,
#        width = 32,
#        units = "cm")

##### N vs. Climate #####

# Join climate and deposition trends
dep_clim_trends_ann <- rbind(ndep_trends_ann, clim_trends)

# Join NO3 and climate trends
no3_clim_trends_ann <- full_join(no3_trends_ann, dep_clim_trends_ann)

# Quick check of overlaps
no3_sites <- no3_trends_ann %>% select(site_code) %>% unique()
clim_sites <- dep_clim_trends_ann %>% select(site_code) %>% unique()
inner <- inner_join(no3_sites, clim_sites) # 148 sites max overlap
only_no3 <- no3_sites %>%
    filter(!site_code %in% inner$site_code) # 53 sites only with NO3 data
# i.e., the PR, Krycklan, McMurdo, Toolik, etc. sites
only_clim <- clim_sites %>%
    filter(!site_code %in% inner$site_code) # 18 sites with only climate data
# i.e., sites part of other regions but missing N data

# Trim to variables of interest
no3_clim_trends_ann_wide <- no3_clim_trends_ann %>%
    select(site_code, var, trend) %>%
    pivot_wider(names_from = var,
                values_from = trend)

# And join with trend flags and number of obs. data
no3_confidence <- no3_trends_ann %>%
    select(site_code, flag, mean_ann_records)

no3_clim_trends_ann_wide <- full_join(no3_clim_trends_ann_wide,
                                      no3_confidence) %>%
    mutate(group = factor(case_when(flag %in% c("increasing", "decreasing",
                                         "non-significant") ~ flag,
                             TRUE ~ "insufficient data"),
                          levels = c("decreasing",
                                     "increasing",
                                     "non-significant",
                                     "insufficient data"))) %>%
    mutate(freq = case_when(group %in% c("insufficient data",
                                           "non-significant") ~ NA,
                              mean_ann_records <= 10 ~ "monthly",
                              mean_ann_records > 10 &
                                  mean_ann_records <= 50 ~ "weekly",
                              mean_ann_records > 50 ~ "subweekly")) %>%
    mutate(infill = factor(case_when(group == "decreasing" & freq == "subweekly" ~ "-, subweekly",
                                     group == "decreasing" & freq == "weekly" ~ "-, weekly",
                                     group == "decreasing" & freq == "monthly" ~ "-, monthly",
                                     group == "increasing" & freq == "subweekly" ~ "+, subweekly",
                                     group == "increasing" & freq == "weekly" ~ "+, weekly",
                                     group == "increasing" & freq == "monthly" ~ "+, monthly",
                                     group == "non-significant" ~ "no trend",
                                     TRUE ~ "insufficient data"),
                           levels = c("-, subweekly","-, weekly","-, monthly",
                                      "no trend","+, monthly","+, weekly","+, subweekly",
                                      "insufficient data")))

# Join with site level data.
no3_clim_trends_ann_wide <- left_join(no3_clim_trends_ann_wide,
                                      ms_site_data)

# Need to order dataset properly before plotting.
no3_clim_trends_ann_wide_ed <- no3_clim_trends_ann_wide %>%
    filter(ws_status == "non-experimental") %>%
    arrange(NO3_N) %>%
    arrange(desc(group))

# PRECIP V TEMP
(figNO3_ppt_temp <- ggplot(no3_clim_trends_ann_wide_ed,
                          aes(x = temp_mean,
                              y = precip_mean)) +
    geom_hline(yintercept = 0, color = "gray20") +
    geom_vline(xintercept = 0, color = "gray20") +
    geom_point(alpha = 0.7,
               aes(shape = group,
                   color = group,
                   size = group)) +
    xlim(-0.06, 0.06) +
    ylim(-0.04, 0.04) +
    annotate("text", x = -0.042, y = 0.04, label = "Cooling, Wetting") +
    annotate("text", x = -0.042, y = -0.04, label = "Cooling, Drying") +
    annotate("text", x = 0.042, y = 0.04, label = "Warming, Wetting") +
    annotate("text", x = 0.042, y = -0.04, label = "Warming, Drying") +
    scale_shape_manual(values = c(20, 20, 4),
                       guide = "none") +
    scale_size_manual(values = c(6, 4, 2),
                       guide = "none") +
    scale_color_manual(values = c("blue", #"royalblue1",
                                  #"cadetblue3",
                                  "gray46",
                                  #"lightsalmon1",
                                  # "coral1",
                                  #"firebrick2",
                                  "gray76"),
                       guide = "none") +
    labs(x = "Mean Annual Temperature Trend",
         y = "Mean Annual Precipitation Trend",
         color = "Trend & Sampling Frequency") +
    theme_bw())

# GPP V TEMP
(figNO3_gpp_temp <- ggplot(no3_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = gpp_CONUS_30m_median)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.001, 0.001) +
        annotate("text", x = -0.04, y = 0.001, label = "Cooling, Greening") +
        annotate("text",x = -0.04, y = -0.001, label = "Cooling, Browning") +
        annotate("text",x = 0.04, y = 0.001, label = "Warming, Greening") +
        annotate("text",x = 0.04, y = -0.001, label = "Warming, Browning") +
        scale_shape_manual(values = c(20, 20, 4)) +
        scale_size_manual(values = c(6, 4, 2)) +
        scale_color_manual(values = c("blue", #"royalblue1",
                                      #"cadetblue3",
                                      "gray46",
                                      #"lightsalmon1",
                                      #"coral1",
                                      #"firebrick2",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend",
             color = "NO3 Trend",
             shape = "NO3 Trend",
             size = "NO3 Trend") +
        theme_bw() +
        theme(legend.position = "bottom"))

# DEP V TEMP
(figNO3_dep_temp <- ggplot(no3_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = N_flux_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.12, 0.12) +
        annotate("text", x = -0.035, y = 0.12, label = "Cooling, Eutrophying") +
        annotate("text",x = -0.035, y = -0.12, label = "Cooling, Oligotrophying") +
        annotate("text",x = 0.035, y = 0.12, label = "Warming, Eutrophying") +
        annotate("text",x = 0.035, y = -0.12, label = "Warming, Oligotrophying") +
        scale_shape_manual(values = c(20, 20, 4), guide = "none") +
        scale_size_manual(values = c(6, 4, 2), guide = "none") +
        scale_color_manual(values = c("blue", #"royalblue1",
                                      #"cadetblue3",
                                      "gray46",
                                      #"lightsalmon1",
                                      # "coral1",
                                      #"firebrick2",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend") +
        theme_bw())

(figNO3_all <- figNO3_ppt_temp + figNO3_gpp_temp + figNO3_dep_temp +
        plot_annotation(tag_levels = "a"))

# ggsave(figNO3_all,
#        filename = "figures/panelfig_no3_clim_dep_trends.jpeg",
#        height = 14,
#        width = 40,
#        units = "cm")

# End of script.
