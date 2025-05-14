# Nitrogen data viz script

#### README ####
# The following script will create sequential
# figures for use in Heili's 2024 AGU talk to
# discuss MacroSheds as a project as well as
# annual & monthly nitrogen trends.

# It will also create figures for the N manuscript.

#### Load packages ####
library(here)
library(ggtext)
library(scales)
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

# All sites and analytes for which we could calculate seasonal VWMs
N_VWM_seasonal <- readRDS("data_working/N_VWM_seasonal.rds")

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

#### Manuscript Figs ####

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

# Test figure.
# Set color legend break points.
my_breaks <- c(1, 10, 50)

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

# Test figure.
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
#        width = 20,
#        units = "cm")

# Now, I also need to create a summary figure of sorts for
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

# This takes a minute or two so be patient.
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

# Trims down climate data.
clim_annual <- clim %>%
    # select only for sites of interest
    filter(site_code %in% annual_sites) %>%
    # calculate annual metrics
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp = mean(temp_median, na.rm = TRUE),
              sum_ann_ppt = sum(precip_median, na.rm = TRUE)) %>%
    ungroup()

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

# Further trims climate data
clim_decadal <- clim_annual %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_temp = mean(mean_ann_temp, na.rm = TRUE),
              mean_sum_ann_ppt = mean(sum_ann_ppt, na.rm = TRUE)) %>%
    ungroup()

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
clim_deponly <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% annual_sites)

clim_deponly20 <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% annual_sites20) %>%
    filter(year > 2009) %>%
    filter(year < 2021)

# Further trims deposition data
dep_decadal <- clim_deponly %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_Ndep = mean(N_flux_mean, na.rm = TRUE)) %>%
    ungroup()

dep_decadal20 <- clim_deponly20 %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_Ndep20 = mean(N_flux_mean, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_dep_data <- full_join(mean_N_VWM_annual_nonexp, dep_decadal)
N_dep_data20 <- full_join(mean_N_VWM_annual20_nonexp, dep_decadal20)

# GPP
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

# Trims down productivity data.
prod_annual <- prod %>%
    filter(site_code %in% annual_sites) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup()

prod_annual20 <- prod %>%
    filter(site_code %in% annual_sites20) %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod20 = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup()

# Further trims productivity data
prod_decadal <- prod_annual %>%
    group_by(site_code) %>%
    summarize(mean_sum_ann_prod = mean(sum_ann_prod, na.rm = TRUE)) %>%
    ungroup()

prod_decadal20 <- prod_annual20 %>%
    group_by(site_code) %>%
    summarize(mean_sum_ann_prod20 = mean(sum_ann_prod20, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_gpp_data <- full_join(mean_N_VWM_annual, prod_decadal)
N_gpp_data20 <- full_join(mean_N_VWM_annual20, prod_decadal20)

# Overall attributes

# Calculated total wetland cover.
ms_ws_select <- ms_ws_attr %>%
    mutate(nlcd_wetland = nlcd_water + nlcd_wetland_herb + nlcd_wetland_wood,
           nlcd_dev = nlcd_dev_hi + nlcd_dev_med + nlcd_dev_low + nlcd_dev_open) %>%
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

##### N vs. Climate #####

# Join climate and deposition trends
dep_clim_trends_ann <- rbind(ndep_trends_ann, clim_trends)

# Join NO3 and climate trends
no3_clim_trends_ann <- full_join(no3_trends_ann, dep_clim_trends_ann)

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

# NO3 V TEMP - removed MCDN
(figNO3_temp <- ggplot(no3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = NO3_N)) +
        geom_point(size = 6,
                   aes(alpha = mean_ann_records)) +
        ylim(-0.0015, 0.0015) +
        scale_alpha_continuous(trans = "log", breaks = c(6, 12, 52, 365)) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual NO3 Trend",
             alpha = "Mean Annual Obs.") +
        theme_bw())

# NO3 V PPT - removed MCDN
(figNO3_ppt <- ggplot(no3_clim_trends_ann_wide,
                       aes(x = precip_mean,
                           y = NO3_N)) +
        geom_point(size = 6,
                   aes(alpha = mean_ann_records)) +
        ylim(-0.0015, 0.0015) +
        scale_alpha_continuous(trans = "log", breaks = c(6, 12, 52, 365)) +
        labs(x = "Mean Annual Precipitation Trend",
             y = "Mean Annual NO3 Trend",
             alpha = "Mean Annual Obs.") +
        theme_bw())

# NO3 V GPP - removed MCDN
(figNO3_gpp <- ggplot(no3_clim_trends_ann_wide,
                      aes(x = gpp_CONUS_30m_median,
                          y = NO3_N)) +
        geom_point(size = 6,
                   aes(alpha = mean_ann_records)) +
        ylim(-0.0015, 0.0015) +
        scale_alpha_continuous(trans = "log", breaks = c(6, 12, 52, 365)) +
        labs(x = "Mean Annual GPP Trend",
             y = "Mean Annual NO3 Trend",
             alpha = "Mean Annual Obs.") +
        theme_bw())

# NO3 V obs - removed MCDN
(figNO3_obs <- ggplot(no3_clim_trends_ann_wide,
                      aes(x = mean_ann_records,
                          y = NO3_N)) +
        geom_point(size = 6,
                   aes(alpha = mean_ann_records)) +
        #ylim(-0.0015, 0.0015) +
        xlim(0, 100) +
        scale_alpha_continuous(trans = "log", breaks = c(6, 12, 52, 365)) +
        labs(x = "Mean Annual Obs.",
             y = "Mean Annual NO3 Trend",
             alpha = "Mean Annual Obs.") +
        theme_bw())

# PRECIP V TEMP
(figNO3_ppt_temp <- ggplot(no3_clim_trends_ann_wide,
                          aes(x = temp_mean,
                              y = precip_mean)) +
    geom_hline(yintercept = 0, color = "gray20") +
    geom_vline(xintercept = 0, color = "gray20") +
    geom_point(size = 6,
               alpha = 0.7,
               aes(shape = group,
                   color = infill)) +
    scale_shape_manual(values = c(20, 20, 20, 4),
                       guide = "none") +
    scale_color_manual(values = c("blue", "royalblue1",
                                  #"cadetblue3",
                                  "gray46",
                                  #"lightsalmon1",
                                  "coral1",
                                  #"firebrick2",
                                  "gray76"),
                       guide = "none") +
    labs(x = "Mean Annual Temperature Trend",
         y = "Mean Annual Precipitation Trend",
         #shape = "Trend & Sampling Frequency",
         color = "Trend & Sampling Frequency") +
    theme_bw())

# GPP V TEMP
(figNO3_gpp_temp <- ggplot(no3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = gpp_CONUS_30m_median)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(size = 6,
                   alpha = 0.7,
                   aes(shape = group,
                       color = infill)) +
        scale_shape_manual(values = c(20, 20, 20, 4),
                           guide = "none") +
        scale_color_manual(values = c("blue", "royalblue1",
                                      #"cadetblue3",
                                      "gray46",
                                      #"lightsalmon1",
                                      "coral1",
                                      #"firebrick2",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual GPP Trend") +
        theme_bw())

# DEP V TEMP
(figNO3_dep_temp <- ggplot(no3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = N_flux_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(size = 6,
                   alpha = 0.7,
                   aes(shape = group,
                       color = infill)) +
        scale_shape_manual(values = c(20, 20, 20, 4),
                           guide = "none") +
        scale_color_manual(values = c("blue", "royalblue1",
                                      #"cadetblue3",
                                      "gray46",
                                      #"lightsalmon1",
                                      "coral1",
                                      #"firebrick2",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend",
             color = "NO3 Trend & Sampling Frequency") +
        theme_bw())

(figNO3_all <- figNO3_ppt_temp + figNO3_gpp_temp + figNO3_dep_temp)

# ggsave(figNO3_all,
#        filename = "figures/panelfig_no3_clim_dep_trends.jpeg",
#        height = 8,
#        width = 30,
#        units = "cm")

# End of script.
