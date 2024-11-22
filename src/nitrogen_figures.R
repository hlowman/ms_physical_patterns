# Nitrogen data viz script

#### README ####
# The following script will create sequential
# figures for use in Heili's 2024 AGU talk to
# discuss MacroSheds as a project as well as
# annual & monthly nitrogen trends

#### Load packages ####
library(here)
library(ggtext)
library(scales)
source(here('src', 'setup.R'))

#### Load data ####

# Annual
# All sites for which we could calculate annual VWM
n_annual_vwm <- readRDS("data_working/nitrogen_annual_VWM.rds")

# Sites that were actually used in annual-scale trend analyses
n_annual_vwm_filtered <- readRDS("data_working/nitrogen_annual_VWM_good.rds")

# And annual trends that were calculated.
n_annual_trends <- readRDS("data_working/nitrogen_annual_trends.rds")

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
     geom_point(size = 2, shape = 1) +
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

# This is the base plot that all others should be built
# around since it includes all possible sites (n = 183).
(fig2 <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing 4 outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40),
                aes(x = water_year,
                    y = annual_vwm_mgL)) +
     geom_point(size = 2, shape = 1, color = "black") +
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

# Color by experimental (n = 36) and non-experimental (n = 143) sites.
(fig3 <- ggplot(n_annual_vwm %>%
                    # removes Bonanza Creek only
                    drop_na(ws_status) %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    color = ws_status)) +
        geom_point(size = 2, shape = 1) +
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
                    filter(annual_vwm_mgL < 40),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    color = keepkeep)) +
     scale_color_manual(values = c("black", "transparent")) +
     geom_point(size = 2, shape = 1) +
     labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
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
                    filter(annual_vwm_mgL < 40),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    group = factor(site_code),
                    fill = keepkeepkeep,
                    color = keepkeepkeep)) +
        geom_point(size = 2, shape = 21) +
        scale_fill_manual(values = c("#55C667FF",
                                      "#404788FF",
                                      "transparent",
                                      "transparent")) +
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

# ggsave(fig5a,
#        filename = "figures/agu_fig5a.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Highlighting positive trends at BES sites.
(fig5a1 <- ggplot(n_annual_vwm %>%
                     filter(analyte == "nitrate_N")%>%
                     # removing outliers for plotting ease
                     filter(annual_vwm_mgL > 0.000001) %>%
                     filter(annual_vwm_mgL < 40),
                 aes(x = water_year,
                     y = annual_vwm_mgL,
                     group = factor(site_code),
                     fill = keepkeepkeep,
                     color = keepkeepkeep)) +
        geom_point(size = 2, shape = 21) +
        scale_fill_manual(values = c("#55C667FF",
                                     "#404788FF",
                                     "transparent",
                                     "transparent")) +
        scale_color_manual(values = c("#55C667FF",
                                      "#404788FF",
                                      "grey70",
                                      "transparent")) +
        geom_smooth(data = n_annual_vwm %>%
                        # create additional column to designate
                        # which lms show
                        mutate(model = factor(case_when(site_code %in% c("BARN",
                                                                         "MCDN") ~ "YES",
                                                        TRUE ~ "NO"),
                                              levels = c("YES", "NO"))) %>%
                        filter(model == "YES"),
                    aes(x = water_year,
                        y = annual_vwm_mgL,
                        group = site_code),
                    method = "lm",
                    se = F,
                    color = "#55C667FF") +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
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

# Highlighting negative trends at LUQ sites.
(fig5a2 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      fill = keepkeepkeep,
                      color = keepkeepkeep)) +
        geom_point(size = 2, shape = 21) +
        scale_fill_manual(values = c("#55C667FF",
                                     "#404788FF",
                                     "transparent",
                                     "transparent")) +
        scale_color_manual(values = c("#55C667FF",
                                      "#404788FF",
                                      "grey70",
                                      "transparent")) +
        geom_smooth(data = n_annual_vwm %>%
                        # create additional column to designate
                        # which lms show
                        mutate(model = factor(case_when(site_code %in% c("Q1",
                                                                         "Q2",
                                                                         "Q3") ~ "YES",
                                                        TRUE ~ "NO"),
                                              levels = c("YES", "NO"))) %>%
                        filter(model == "YES"),
                    aes(x = water_year,
                        y = annual_vwm_mgL,
                        group = site_code),
                    method = "lm",
                    se = F,
                    color = "#404788FF") +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
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
                      filter(annual_vwm_mgL < 40),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      fill = keepkeepkeep,
                      color = keepkeepkeep)) +
        geom_point(size = 2, shape = 21) +
        scale_fill_manual(values = c("#55C667FF",
                                     "#404788FF",
                                     "transparent",
                                     "transparent")) +
        scale_color_manual(values = c("#55C667FF",
                                      "#404788FF",
                                      "grey70",
                                      "transparent")) +
        geom_smooth(data = n_annual_vwm %>%
                        # create additional column to designate
                        # which lms show
                        mutate(model = factor(case_when(site_code %in% c("w3",
                                                                         "w6",
                                                                         "w7",
                                                                         "w8",
                                                                         "w9") ~ "YES",
                                                        TRUE ~ "NO"),
                                              levels = c("YES", "NO"))) %>%
                        filter(model == "YES"),
                    aes(x = water_year,
                        y = annual_vwm_mgL,
                        group = site_code),
                    method = "lm",
                    se = F,
                    color = "#404788FF") +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a3,
#        filename = "figures/agu_fig5a3.jpeg",
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

# End of script.
