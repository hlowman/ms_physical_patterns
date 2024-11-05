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

# End of script.
