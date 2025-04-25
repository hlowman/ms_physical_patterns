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
n_annual_vwm <- readRDS("data_working/nitrogen_annual_VWM.rds")

# Sites that were actually used in annual-scale trend analyses
n_annual_vwm_filtered <- readRDS("data_working/nitrogen_annual_VWM_good.rds")

# And annual trends that were calculated.
n_annual_trends <- readRDS("data_working/nitrogen_annual_trends.rds")

# Including monthly data.
n_monthly_vwm <- readRDS("data_working/nitrogen_monthly_VWM.rds")

n_monthly_vwm_filtered <- readRDS("data_working/nitrogen_monthly_VWM_good.rds")

# As well as monthly trends.
n_monthly_trends <- readRDS("data_working/nitrogen_monthly_trends.rds")

# Datasets for MANUSCRIPT figures:
# Annual
# All sites and analytes for which we could calculate annual VWMs
N_VWM_annual <- readRDS("data_working/N_VWM_annual.rds")

# NO3 annual trends
no3_trends_ann <- readRDS("data_working/no3_trends_annual.rds")

# Climate trends (from mega_zipper_data.R script)
clim_trends <- read_csv("data_working/trends/full_prisim_climate.csv")

#### Figures ####

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
    summarize(mean_annual_VWM_mgLha = mean(annual_vwm_mgLha,
                                           na.rm = TRUE),
              mean_annual_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Test figure.
# Set color legend break points.
my_breaks <- c(1, 10, 50)

(summaryfig1 <- ggplot(mean_N_VWM_annual,
                      aes(x = mean_annual_VWM_mgLha,
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
    labs(x = "Mean Annual Concentration (mg/L*ha)",
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
# BUT FILTER DOWN TO 2010-2020
mean_N_VWM_annual20 <- N_VWM_annual %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, analyte_N) %>%
    summarize(mean_annual_VWM_mgLha = mean(annual_vwm_mgLha,
                                           na.rm = TRUE),
              mean_annual_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Test figure.
# Set color legend break points.
color_breaks <- c(1, 10, 50)
size_breaks <- c(1,5,10)

(summaryfig4 <- ggplot(mean_N_VWM_annual20,
                       aes(x = mean_annual_VWM_mgLha,
                           y = analyte_N,
                           fill = mean_annual_obs,
                           size = total_years)) +
        geom_jitter(width = 0.05, alpha = 0.9, shape = 21) +
        scale_fill_gradientn(colors = c("white", "#FAB455",
                                        "#69B9FA", "#59A3F8",
                                        "#4B9FF7", "#045CB4", "black"),
                             trans = "log",
                             breaks = color_breaks, labels = color_breaks) +
        scale_size_continuous(breaks = size_breaks) +
        scale_x_log10() +
        facet_grid(analyte_N~., scales = "free") +
        labs(x = "Mean Annual Concentration (mg/L*ha)",
             y = "Analyte",
             fill = "Mean No. of Annual Observations",
             size = "Record Length (yrs)") +
        theme_bw() +
        theme(strip.text.y = element_blank(),
              legend.position = "top"))

# ggsave(summaryfig4,
#        filename = "figures/summaryfig_N_2010_to_2020.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

site_counts20 <- mean_N_VWM_annual20 %>%
    count(analyte_N)

##### Site Attributes #####

# Also going to make plots to investigate watershed and climate
# characteristics with magnitude of nitrogen at each site.
# Focusing in on 2010-2020 data to keep things comparable.

# First need to make a site list.
annual_sites <- unique(mean_N_VWM_annual20$site_code)

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
    # select for only timeframe of interest (2010-2020)
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    # calculate annual metrics
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp = mean(temp_median, na.rm = TRUE),
              sum_ann_ppt = sum(precip_median, na.rm = TRUE)) %>%
    ungroup()

# Further trims climate data
clim_decadal <- clim_annual %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_temp = mean(mean_ann_temp, na.rm = TRUE),
              mean_sum_ann_ppt = mean(sum_ann_ppt, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_clim_data20 <- full_join(mean_N_VWM_annual20, clim_decadal)

# Temperature
(summaryfig_Temp <- ggplot(N_clim_data20,
                       aes(x = mean_mean_ann_temp,
                           y = mean_annual_VWM_mgLha)) +
        geom_point(fill = "#D46F10", alpha = 0.9,
                   size = 2, shape = 21) +
        scale_y_log10() +
        facet_wrap(vars(analyte_N)) +
        labs(x = "Mean Annual Temperature (C)",
             y = "Mean Annual Concentration (mg/L*ha)") +
        theme_bw() +
        theme(legend.position = "top"))

# ggsave(summaryfig_Temp,
#        filename = "figures/summaryfig_N_Temp_2010_to_2020.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

# Precip
(summaryfig_Ppt <- ggplot(N_clim_data20,
                           aes(x = mean_sum_ann_ppt,
                               y = mean_annual_VWM_mgLha)) +
        geom_point(fill = "#4B8FF7", alpha = 0.9,
                   size = 2, shape = 21) +
        scale_y_log10() +
        facet_wrap(vars(analyte_N)) +
        labs(x = "Mean Annual Cumulative Precipitation (mm)",
             y = "Mean Annual Concentration (mg/L*ha)") +
        theme_bw() +
        theme(legend.position = "top"))

# ggsave(summaryfig_Ppt,
#        filename = "figures/summaryfig_N_Ppt_2010_to_2020.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

# N deposition
# New dataset since deposition is already annualized
clim_deponly <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% annual_sites) %>%
    filter(year > 2009) %>%
    filter(year < 2021)

# Further trims deposition data
dep_decadal <- clim_deponly %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_Ndep = mean(N_flux_mean, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_dep_data20 <- full_join(mean_N_VWM_annual20, dep_decadal)

(summaryfig_Ndep <- ggplot(N_dep_data20,
                          aes(x = mean_mean_ann_Ndep,
                              y = mean_annual_VWM_mgLha)) +
        geom_point(fill = "#E29244", alpha = 0.9,
                   size = 2, shape = 21) +
        scale_y_log10() +
        facet_wrap(vars(analyte_N)) +
        labs(x = "Mean Annual Cumulative N Deposition (kg/ha)",
             y = "Mean Annual Concentration (mg/L*ha)") +
        theme_bw() +
        theme(legend.position = "top"))
#
# ggsave(summaryfig_Ndep,
#        filename = "figures/summaryfig_N_Ndep_2010_to_2020.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

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
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup()

# Further trims productivity data
prod_decadal <- prod_annual %>%
    group_by(site_code) %>%
    summarize(mean_sum_ann_prod = mean(sum_ann_prod, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_gpp_data20 <- full_join(mean_N_VWM_annual20, prod_decadal)

(summaryfig_GPP<- ggplot(N_gpp_data20,
                           aes(x = mean_sum_ann_prod,
                               y = mean_annual_VWM_mgLha)) +
        geom_point(fill = "#368000", alpha = 0.9,
                   size = 2, shape = 21) +
        scale_y_log10() +
        facet_wrap(vars(analyte_N)) +
        labs(x = "Mean Cumulative Annual GPP (kg C/m<sup>2</sup>)",
             y = "Mean Annual Concentration (mg/L*ha)") +
        theme_bw() +
        theme(axis.title.x = element_markdown(),
              legend.position = "top"))

# ggsave(summaryfig_GPP,
#        filename = "figures/summaryfig_N_GPP_2010_to_2020.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

# Overall attributes

# Calculated total wetland cover.
ms_ws_select <- ms_ws_attr %>%
    mutate(nlcd_wetland = nlcd_water + nlcd_wetland_herb + nlcd_wetland_wood,
           nlcd_dev = nlcd_dev_hi + nlcd_dev_med + nlcd_dev_low + nlcd_dev_open) %>%
    select(network, domain, site_code, area, slope_mean, elev_mean,
           nlcd_dev, nlcd_wetland)

# Join watershed attributes with N data.
N_ws_data20 <- full_join(mean_N_VWM_annual20, ms_ws_select)

# Realizing it likely makes more sense to plot overall distribution
# of attributes rather than by analyte, so going to work on that below.

# Aggregate all datasets together.
N_data20 <- dep_decadal %>% filter(site_code %in% annual_sites)
N_data20 <- full_join(N_data20, prod_decadal)
N_data20 <- full_join(N_data20, clim_decadal)
N_data20 <- full_join(N_data20, ms_ws_select)
N_data20 <- N_data20 %>%
    mutate(group = "MacroSheds")

(deppanel <- ggplot(N_data20,
                       aes(x = mean_mean_ann_Ndep,
                           y = group)) +
        geom_jitter(fill = "#E29244", alpha = 0.9, shape = 21,
                    width = 0.05, size = 2) +
        labs(x = "Mean Annual Cumulative N Deposition (kg/ha)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(deppanelv2 <- ggplot(N_data20,
                    aes(x = mean_mean_ann_Ndep)) +
        geom_density(fill = "#E29244", alpha = 0.7) +
        labs(x = "Annual Cumulative N Dep. (kg/ha)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(temppanel <- ggplot(N_data20,
                    aes(x = mean_mean_ann_temp,
                        y = group)) +
        geom_jitter(fill = "#D46F10", alpha = 0.9, shape = 21,
                    width = 0.05, size = 2) +
        labs(x = "Mean Annual Temperature (C)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(temppanelv2 <- ggplot(N_data20,
                     aes(x = mean_mean_ann_temp)) +
        geom_density(fill = "#D46F10", alpha = 0.7) +
        labs(x = "Annual Temperature (C)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(gpppanel <- ggplot(N_data20,
                     aes(x = mean_sum_ann_prod,
                         y = group)) +
        geom_jitter(fill = "#4CA49E", alpha = 0.9, shape = 21,
                    width = 0.05, size = 2) +
        labs(x = "Mean Cumulative Annual GPP (kg C/m<sup>2</sup>)") +
        theme_bw() +
        theme(axis.title.x = element_markdown(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(gpppanelv2 <- ggplot(N_data20,
                    aes(x = mean_sum_ann_prod)) +
        geom_density(fill = "#4CA49E", alpha = 0.7) +
        labs(x = "Cumulative Annual GPP (kg C/m<sup>2</sup>)") +
        theme_bw() +
        theme(axis.title.x = element_markdown(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(pptpanel <- ggplot(N_data20,
                     aes(x = mean_sum_ann_ppt,
                         y = group)) +
         geom_jitter(fill =  "#69B9FA", alpha = 0.9, shape = 21,
                     width = 0.05, size = 2) +
         labs(x = "Mean Cumulative Annual PPT (mm)") +
         theme_bw() +
         theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank()))

(pptpanelv2 <- ggplot(N_data20,
                    aes(x = mean_sum_ann_ppt)) +
        geom_density(fill =  "#69B9FA", alpha = 0.7) +
        labs(x = "Cumulative Annual PPT (mm)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(sizepanel <- ggplot(N_data20,
                    aes(x = area,
                        y = group)) +
        geom_jitter(fill =  "#59A3F8", alpha = 0.9, shape = 21,
                    width = 0.05, size = 2) +
        scale_x_log10() +
        labs(x = "Watershed Area (ha)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(sizepanelv2 <- ggplot(N_data20,
                     aes(x = area)) +
        geom_density(fill =  "#59A3F8", alpha = 0.7) +
        scale_x_log10() +
        labs(x = "Watershed Area (ha)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(slopepanel <- ggplot(N_data20,
                     aes(x = slope_mean,
                         y = group)) +
        geom_jitter(fill =  "#4B8FF7", alpha = 0.9, shape = 21,
                    width = 0.05, size = 2) +
        labs(x = "Mean Watershed Slope (degrees)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(slopepanelv2 <- ggplot(N_data20,
                      aes(x = slope_mean)) +
        geom_density(fill =  "#4B8FF7", alpha = 0.7) +
        labs(x = "Watershed Slope (degrees)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(elevpanel <- ggplot(N_data20,
                      aes(x = elev_mean,
                          y = group)) +
        geom_jitter(fill =  "#5A7ECB", alpha = 0.9, shape = 21,
                    width = 0.05, size = 2) +
        labs(x = "Mean Watershed Elevation (m)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(elevpanelv2 <- ggplot(N_data20,
                     aes(x = elev_mean)) +
        geom_density(fill =  "#5A7ECB", alpha = 0.7) +
        labs(x = "Watershed Elevation (m)") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(wetlandpanel <- ggplot(N_data20,
                     aes(x = nlcd_wetland,
                         y = group)) +
        geom_jitter(fill =  "#6B6D9F", alpha = 0.9, shape = 21,
                    width = 0.05, size = 2) +
        scale_x_log10() +
        labs(x = "% Wetland Cover") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(wetlandpanelv2 <- ggplot(N_data20,
                        aes(x = nlcd_wetland)) +
        geom_density(fill =  "#6B6D9F", alpha = 0.7) +
        scale_x_log10() +
        labs(x = "% Wetland Cover") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(devpanel <- ggplot(N_data20,
                        aes(x = nlcd_dev,
                            y = group)) +
        geom_jitter(fill =  "#1E2F46", alpha = 0.9, shape = 21,
                    width = 0.05, size = 2) +
        scale_x_log10() +
        labs(x = "% Developed") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(devpanelv2 <- ggplot(N_data20,
                    aes(x = nlcd_dev)) +
        geom_density(fill =  "#1E2F46", alpha = 0.7) +
        scale_x_log10() +
        labs(x = "% Developed") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()))

(ws_attributes <- deppanel /
    temppanel/
    gpppanel/
    pptpanel/
    sizepanel/
    slopepanel/
    elevpanel/
    wetlandpanel/
    devpanel)

(ws_attributes_v2 <- (deppanelv2 + temppanelv2 + pptpanelv2)/
        (gpppanelv2 + wetlandpanelv2 + devpanelv2)/
        (sizepanelv2 + slopepanelv2 + elevpanelv2))


# ggsave(ws_attributes,
#        filename = "figures/summaryfig_attributes_2010_to_2020.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

# ggsave(ws_attributes_v2,
#        filename = "figures/summaryfig_attributes_dens_2010_to_2020.jpeg",
#        height = 14,
#        width = 20,
#        units = "cm")

##### N vs. Climate #####

# Join NO3 and climate trends
no3_clim_trends_ann <- full_join(no3_trends_ann, clim_trends)

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
    mutate(infill = case_when(group %in% c("insufficient data",
                                           "non-significant")~ NA,
                              TRUE ~ mean_ann_records))

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
    geom_point(size = 6,
               aes(shape = group,
                   color = group,
                   alpha = infill)) +
    scale_shape_manual(values = c(20, 20, 21, 4)) +
    scale_color_manual(values = c("purple","cyan4","darkorange", "grey80")) +
    scale_alpha_continuous(trans = "log", breaks = c(6, 12, 52, 365)) +
    labs(x = "Mean Annual Temperature Trend",
         y = "Mean Annual Precipitation Trend",
         shape = "NO3 Trend",
         color = "NO3 Trend",
         alpha = "Mean Annual Obs.") +
    theme_bw() +
    theme(legend.position = "none"))

# GPP V TEMP
(figNO3_gpp_temp <- ggplot(no3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = gpp_CONUS_30m_median)) +
        geom_point(size = 6,
                   aes(shape = group,
                       color = group,
                       alpha = infill)) +
        scale_shape_manual(values = c(20, 20, 21, 4)) +
        scale_color_manual(values = c("purple","cyan4","darkorange", "grey80")) +
        scale_alpha_continuous(trans = "log", breaks = c(6, 12, 52, 365)) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual GPP Trend",
             shape = "NO3 Trend",
             color = "NO3 Trend",
             alpha = "Mean Annual Obs.") +
        theme_bw())

(figNO3_all <- figNO3_ppt_temp + figNO3_gpp_temp)

# ggsave(figNO3_all,
#        filename = "figures/panelfig_no3_clim_trends.jpeg",
#        height = 8,
#        width = 20,
#        units = "cm")

# End of script.
