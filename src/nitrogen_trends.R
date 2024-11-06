# Nitrogen data trends script

#### README ####
# The following script will estimate trends
# for nitrogen data. It will also create
# accompanying data visualizations.

#### Load packages ####
library(here)
source(here('src', 'setup.R'))

#### Load data ####

# Annual volume weighted means
n_annual <- readRDS("data_working/nitrogen_annual_VWM_good.rds")

# Monthly volume weighted means
n_monthly <- readRDS("data_working/nitrogen_monthly_VWM_good.rds")

#### Trends ####

# Calculate annual trends using sen slope.
n_annual_trends <- n_annual %>%
    # make sure things are in order
    arrange(site_code, analyte, water_year) %>%
    group_by(site_code, analyte) %>%
    # extract sen slopes and significance
    summarize(sen_slope = signif(sens.slope(annual_vwm_mgL)$estimates[[1]],
                                 digits = 3),
              sen_p = sens.slope(annual_vwm_mgL)$p.value) %>%
    ungroup() %>%
    # add columns for identification
    mutate(sig = case_when(sen_p < 0.05 ~ 1,
                           TRUE ~ 0)) %>%
    mutate(trend = factor(case_when(sig == 1 & sen_slope > 0 ~ "sig. increasing",
                                    sig == 0 & sen_slope > 0 ~ "ns. increasing",
                                    sig == 1 & sen_slope < 0 ~ "sig. decreasing",
                                    sig == 0 & sen_slope < 0 ~ "ns. decreasing",
                                    TRUE ~ "no trend"),
                          levels = c("sig. increasing", "ns. increasing",
                                     "no trend",
                                     "ns. decreasing", "sig. decreasing")))

n_annual_trends_summary <- n_annual_trends %>%
    count(analyte, trend) %>%
    ungroup()

(fig_annual <- ggplot(n_annual_trends_summary,
                       aes(x = analyte, y = n, fill = trend)) +
        scale_fill_manual(values = c("red", "darksalmon", "cadetblue3", "blue")) +
        geom_bar(position = "stack", stat = "identity") +
        labs(x = "Analyte", y = "Site Count", fill = "Trend"))

# ggsave(fig_annual,
#        filename = "figures/n_vwm_annual_trends.jpg",
#        width = 12,
#        height = 8,
#        units = "cm")

# Calculate monthly trends using sen slope.
n_monthly_trends <- n_monthly %>%
    # remove a few NaNs that made it in
    filter(monthly_vwm_mgL >= 0) %>%
    # make sure things are in order
    arrange(site_code, analyte, month, year) %>%
    group_by(site_code, analyte, month) %>%
    # extract sen slopes and significance
    summarize(sen_slope = signif(sens.slope(monthly_vwm_mgL)$estimates[[1]],
                                 digits = 3),
              sen_p = sens.slope(monthly_vwm_mgL)$p.value) %>%
    ungroup() %>%
    # add columns for identification
    mutate(sig = case_when(sen_p < 0.05 ~ 1,
                           TRUE ~ 0)) %>%
    mutate(trend = factor(case_when(sig == 1 & sen_slope > 0 ~ "sig. increasing",
                                    sig == 0 & sen_slope > 0 ~ "ns. increasing",
                                    sig == 1 & sen_slope < 0 ~ "sig. decreasing",
                                    sig == 0 & sen_slope < 0 ~ "ns. decreasing",
                                    TRUE ~ "no trend"),
                          levels = c("sig. increasing", "ns. increasing",
                                     "no trend",
                                     "ns. decreasing", "sig. decreasing")))

n_monthly_trends_summary <- n_monthly_trends %>%
    count(analyte, month, trend) %>%
    ungroup()

(fig_monthly <- ggplot(n_monthly_trends_summary %>%
           filter(analyte %in% c("nitrate_N", "ammonia_N",
                                 "TDN", "TN")),
       aes(x = month, y = n, fill = trend)) +
    scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
    scale_fill_manual(values = c("red", "darksalmon", "gray59", "cadetblue3", "blue")) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Month", y = "Site Count", fill = "Trend") +
    facet_wrap(analyte~., scales = "free"))

# ggsave(fig_monthly,
#        filename = "figures/n_vwm_monthly_trends.jpg",
#        width = 20,
#        height = 10,
#        units = "cm")

#### Seasonality ####

# Also curious to examine seasonal/monthly means over the full record
# for all sites.
n_monthly_means <- n_monthly %>%
    # calculate monthly means
    group_by(site_code, analyte, month) %>%
    summarize(mean_vwm = mean(monthly_vwm_mgL, na.rm = TRUE),
              sd_vwm = sd(monthly_vwm_mgL, na.rm = TRUE)) %>%
    ungroup()

n_peak_means <- n_monthly_means %>%
    # and determine what season peak concentrations occur in
    group_by(site_code, analyte) %>%
    slice_max(mean_vwm) %>%
    select(site_code, analyte, month) %>%
    rename(peak_month = month) %>%
    ungroup()

# Trim down metadata for using domains.
domains <- ms_site_data %>%
    select(domain, site_code)

n_monthly_means <- full_join(n_monthly_means, n_peak_means) %>%
    left_join(., domains) %>%
    mutate(peak_season = factor(case_when(peak_month %in% c(9,10,11) ~ "Fall",
                                   peak_month %in% c(12,1,2) ~ "Winter",
                                   peak_month %in% c(3,4,5) ~ "Spring",
                                   peak_month %in% c(6,7,8) ~ "Summer"),
                                levels = c("Fall", "Winter",
                                           "Spring", "Summer")))

(fig_seasonal1 <- ggplot(n_monthly_means %>%
                             filter(analyte %in% c("nitrate_N",
                                                   "ammonia_N",
                                                   "TDN",
                                                   "TN")),
                  aes(x = peak_month, fill = domain)) +
        scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
        scale_fill_viridis(discrete = TRUE) +
        geom_bar(position = "stack") +
        theme_bw() +
        facet_wrap(analyte~.))

# ggsave(fig_seasonal1,
#        filename = "figures/n_vwm_monthly_peaks.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

(fig_seasonal2 <- ggplot(n_monthly_means %>%
                             filter(analyte == "nitrate_N"),
                         aes(x = month,
                             y = mean_vwm,
                             group = site_code,
                             color = domain)) +
        scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
        scale_color_viridis(discrete = TRUE) +
        geom_line(linewidth = 1) +
        theme_bw() +
        facet_wrap(peak_season~., scales = "free"))

# ggsave(fig_seasonal2,
#        filename = "figures/n_vwm_seasonal.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

nitrate_monthly_means_summary <- n_monthly_means %>%
    filter(analyte == "nitrate_N") %>%
    group_by(site_code) %>%
    slice_head() %>%
    ungroup() %>%
    count(peak_season) %>%
    ungroup()

# End of script.
