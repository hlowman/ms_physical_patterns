# Nitrogen data trends script

#### README ####
# The following script will estimate trends
# for nitrogen data.

#### Load packages ####
library(here)
source(here('src', 'setup.R'))

#### Load data ####

# Load in climate data (for N deposition).
clim_raw <- read_feather(here('data_raw/ms/v2/spatial_timeseries_climate.feather'))

# Load VWM nitrogen datasets.
n_vwm_month <- readRDS("data_working/N_VWM_monthly.rds")
n_vwm_seas <- readRDS("data_working/N_VWM_seasonal.rds")
n_vwm_ann <- readRDS("data_working/N_VWM_annual.rds")

# Tidy column names.
n_vwm_month <- n_vwm_month %>%
    rename(analyte = analyte_N,
           vwm_mgL = monthly_vwm_mgL,
           vwm_mgLha = monthly_vwm_mgLha) %>%
    mutate(timestep = case_when(month == 1 ~ "January",
                                month == 2 ~ "February",
                                month == 3 ~ "March",
                                month == 4 ~ "April",
                                month == 5 ~ "May",
                                month == 6 ~ "June",
                                month == 7 ~ "July",
                                month == 8 ~ "August",
                                month == 9 ~ "September",
                                month == 10 ~ "October",
                                month == 11 ~ "November",
                                month == 12 ~ "December")) %>%
    select(-month)

n_vwm_seas <- n_vwm_seas %>%
    rename(analyte = analyte_N,
           water_year = season_year,
           timestep = season,
           vwm_mgL = seasonal_vwm_mgL,
           vwm_mgLha = seasonal_vwm_mgLha)

n_vwm_ann <- n_vwm_ann %>%
    rename(analyte = analyte_N,
           vwm_mgL = annual_vwm_mgL,
           vwm_mgLha = annual_vwm_mgLha) %>%
    mutate(timestep = "Annual")

# Join all VWM nitrogen data together.
n_vwm <- full_join(n_vwm_ann, n_vwm_month)
n_vwm <- full_join(n_vwm, n_vwm_seas)

# Trim all nitrogen data to only calculate trends post 1980.
n_vwm80 <- n_vwm %>%
    filter(water_year > 1979)

# And only include timesteps with a minimum of 10 observations.
n_vwm80_10 <- n_vwm80 %>%
    filter(n_of_obs_chem > 9)

# Also, making a more restrictive filter for minimum 1 obs. in
# 8 months of a given year, using the monthly data.
n_month_counts <- n_vwm %>%
    filter(timestep %in% c("January", "February", "March", "April",
                           "May", "June", "July", "August",
                           "September", "October", "November", "December")) %>%
    select(site_code, analyte, water_year, timestep, n_of_obs_chem) %>%
    pivot_wider(values_from = n_of_obs_chem,
                names_from = timestep) %>%
    # reordering just for ease of viewing
    select(site_code, analyte, water_year, January, February, March, April,
           May, June, July, August, September, October, November, December) %>%
    # now to add appropriate flags
    mutate(flag_10 = case_when(October >= 1 ~ 1,
                               TRUE ~ 0),
           flag_11 = case_when(November >= 1 ~ 1,
                               TRUE ~ 0),
           flag_12 = case_when(December >= 1 ~ 1,
                               TRUE ~ 0),
           flag_1 = case_when(January >= 1 ~ 1,
                              TRUE ~ 0),
           flag_2 = case_when(February >= 1 ~ 1,
                              TRUE ~ 0),
           flag_3 = case_when(March >= 1 ~ 1,
                              TRUE ~ 0),
           flag_4 = case_when(April >= 1 ~ 1,
                              TRUE ~ 0),
           flag_5 = case_when(May >= 1 ~ 1,
                              TRUE ~ 0),
           flag_6 = case_when(June >= 1 ~ 1,
                              TRUE ~ 0),
           flag_7 = case_when(July >= 1 ~ 1,
                              TRUE ~ 0),
           flag_8 = case_when(August >= 1 ~ 1,
                              TRUE ~ 0),
           flag_9 = case_when(September >= 1 ~ 1,
                              TRUE ~ 0)) %>%
    mutate(flag_months = flag_10 + flag_11 + flag_12 +
               flag_1 + flag_2 + flag_3 +
               flag_4 + flag_5 + flag_6 +
               flag_7 + flag_8 + flag_9)

# And only include timesteps with a minimum of 8 months
# of observations per water year.
n_mos_yrs_trim <- n_month_counts %>%
    filter(flag_months >= 8) %>%
    select(site_code, analyte, water_year, flag_months)

n_vwm80_trim <- right_join(n_vwm80, n_mos_yrs_trim)

#### NO3 ####

##### Annual #####

# Reduce to best range.
no3_vwm_ann <- gen_reduce_to_best_range(data_in = n_vwm80_trim,
                                        solute = 'NO3_N',
                                        aggregation = 'Annual')

# Renaming a column to work with the function below.
no3_vwm_ann <- no3_vwm_ann %>%
    rename(var = analyte) %>%
    rename(val = vwm_mgLha) %>%
    drop_na(var)

# Detect trends.
no3_trends_ann <- detect_trends(no3_vwm_ann)

# Add columns for identification.
no3_trends_ann <- no3_trends_ann %>%
    mutate(flag = case_when(sign(trend_upper)!= sign(trend_lower) ~ 'non-significant',
                            sign(trend_upper) == sign(trend_lower) & sign(trend) > 0 ~ 'increasing',
                            sign(trend_upper) == sign(trend_lower) & sign(trend) < 0 ~ 'decreasing',
                            sign(trend_upper)!= sign(trend_lower) & trend == 0 ~ 'flat',
                            trend == NA ~ 'insufficient data'))

# Calculate average number of records per year per site.
no3_records <- no3_vwm_ann %>%
    group_by(site_code) %>%
    summarize(mean_ann_records = mean(n_of_obs_chem)) %>%
    ungroup()

# And join with primary data frame.
no3_trends_ann <- left_join(no3_trends_ann, no3_records)

all_data_counts_no3_trim <- no3_trends_ann %>%
    count(flag)

# Export for use in making figures.
saveRDS(no3_trends_ann, "data_working/no3_trends_annual.rds")

#### NH4 ####

##### Annual #####

# Reduce to best range.
nh3_vwm_ann <- gen_reduce_to_best_range(data_in = n_vwm80_trim,
                                        solute = 'NH3_N',
                                        aggregation = 'Annual')

# Renaming a column to work with the function below.
nh3_vwm_ann <- nh3_vwm_ann %>%
    rename(var = analyte) %>%
    rename(val = vwm_mgLha) %>%
    drop_na(var)

# Detect trends.
nh3_trends_ann <- detect_trends(nh3_vwm_ann)

# Add columns for identification.
nh3_trends_ann <- nh3_trends_ann %>%
    mutate(flag = case_when(sign(trend_upper)!= sign(trend_lower) ~ 'non-significant',
                            sign(trend_upper) == sign(trend_lower) & sign(trend) > 0 ~ 'increasing',
                            sign(trend_upper) == sign(trend_lower) & sign(trend) < 0 ~ 'decreasing',
                            sign(trend_upper)!= sign(trend_lower) & trend == 0 ~ 'flat',
                            trend == NA ~ 'insufficient data'))

# Calculate average number of records per year per site.
nh3_records <- nh3_vwm_ann %>%
    group_by(site_code) %>%
    summarize(mean_ann_records = mean(n_of_obs_chem)) %>%
    ungroup()

# And join with primary data frame.
nh3_trends_ann <- left_join(nh3_trends_ann, nh3_records)

all_data_counts_nh3_trim <- nh3_trends_ann %>%
    count(flag)

# Export for use in making figures.
saveRDS(nh3_trends_ann, "data_working/nh3_trends_annual.rds")

#### TDN ####

##### Annual #####

# Reduce to best range.
tdn_vwm_ann <- gen_reduce_to_best_range(data_in = n_vwm80_trim,
                                        solute = 'TDN',
                                        aggregation = 'Annual')

# Renaming a column to work with the function below.
tdn_vwm_ann <- tdn_vwm_ann %>%
    rename(var = analyte) %>%
    rename(val = vwm_mgLha) %>%
    drop_na(var)

# Detect trends.
tdn_trends_ann <- detect_trends(tdn_vwm_ann)

# Add columns for identification.
tdn_trends_ann <- tdn_trends_ann %>%
    mutate(flag = case_when(sign(trend_upper)!= sign(trend_lower) ~ 'non-significant',
                            sign(trend_upper) == sign(trend_lower) & sign(trend) > 0 ~ 'increasing',
                            sign(trend_upper) == sign(trend_lower) & sign(trend) < 0 ~ 'decreasing',
                            sign(trend_upper)!= sign(trend_lower) & trend == 0 ~ 'flat',
                            trend == NA ~ 'insufficient data'))

# Calculate average number of records per year per site.
tdn_records <- tdn_vwm_ann %>%
    group_by(site_code) %>%
    summarize(mean_ann_records = mean(n_of_obs_chem)) %>%
    ungroup()

# And join with primary data frame.
tdn_trends_ann <- left_join(tdn_trends_ann, tdn_records)

all_data_counts_tdn_trim <- tdn_trends_ann %>%
    count(flag)

# Export for use in making figures.
saveRDS(tdn_trends_ann, "data_working/tdn_trends_annual.rds")

#### N Deposition ####

##### Annual #####

# Need to select for annualized deposition values.
clim_dep <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    rename("analyte" = "var",
           "water_year" = "year") %>%
    mutate(timestep = "Annual") %>%
    select(-date) # Remove this otherwise the function below trims ALL rows with NAs

# These data start in 1985, so not doing any additional trimming here.
# Reduce to best range.
ndep_ann <- gen_reduce_to_best_range(data_in = clim_dep,
                                     solute = 'N_flux_mean',
                                     aggregation = 'Annual')

# Renaming a column to work with the function below.
ndep_ann <- ndep_ann %>%
    rename(var = analyte) %>%
    drop_na(var)

# Detect trends.
ndep_trends_ann <- detect_trends(ndep_ann)

# Export for use in making figures.
saveRDS(ndep_trends_ann, "data_working/ndep_trends_annual.rds")

#### Old Seasonal Data Exploration Below ####

# Also curious to examine seasonal/monthly means over the full record
# for all sites.
n_monthly_means <- n_monthly %>%
    # calculate monthly means
    group_by(site_code, var, month) %>%
    summarize(mean_vwm = mean(val, na.rm = TRUE),
              sd_vwm = sd(val, na.rm = TRUE)) %>%
    ungroup()

n_peak_means <- n_monthly_means %>%
    # and determine what season peak concentrations occur in
    group_by(site_code, var) %>%
    slice_max(mean_vwm) %>%
    select(site_code, var, month) %>%
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
                             filter(var %in% c("nitrate_N",
                                                   "ammonia_N",
                                                   "TDN",
                                                   "TN")) %>%
                             # need to filter out multiples
                             select(site_code, var, peak_month, domain) %>%
                             distinct(),
                  aes(x = peak_month, fill = domain)) +
        scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
        scale_fill_viridis(discrete = TRUE) +
        geom_bar(position = "stack") +
        theme_bw() +
        facet_wrap(var~.))

# ggsave(fig_seasonal1,
#        filename = "figures/n_vwm_monthly_peaks.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

(fig_seasonal2 <- ggplot(n_monthly_means %>%
                             filter(var == "nitrate_N"),
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
    filter(var == "nitrate_N") %>%
    group_by(site_code) %>%
    slice_head() %>%
    ungroup() %>%
    count(peak_season) %>%
    ungroup()

# Also going back to raw discharge data to calculate
# mean monthly discharge (including all possible data
# since there are far fewer restrictions on this).

# For normalization by watershed area.
# Normalize by watershed area.
area <- ms_site_data %>%
    select(site_code, ws_area_ha)

q_monthly <- dplyr::distinct(q_data, site_code, date, .keep_all = TRUE) %>%
    mutate(month = month(date)) %>%
    # Removes any interpolated values.
    filter(ms_interp == 0) %>%
    # Adds column in L/s*ha
    left_join(area, by = c("site_code")) %>%
    mutate(val_Lsecha = val/ws_area_ha) %>%
    # And summarize values by month
    group_by(site_code, month) %>%
    summarize(mean_q_Lsha = mean(val_Lsecha)) %>%
    ungroup()

# Identify peak seasons.
q_monthly_peaks <- q_monthly %>%
    group_by(site_code) %>%
    slice_max(mean_q_Lsha) %>%
    ungroup() %>%
    rename(peak_month_q = month) %>%
    mutate(peak_season_q = factor(case_when(peak_month_q %in% c(9,10,11) ~ "Fall",
                                            peak_month_q %in% c(12,1,2) ~ "Winter",
                                            peak_month_q %in% c(3,4,5) ~ "Spring",
                                            peak_month_q %in% c(6,7,8) ~ "Summer"),
                                  levels = c("Fall", "Winter",
                                             "Spring", "Summer")))

# Plot these for comparison with N vwm peaks.
# NOTE THIS IS AT ALL SITES - NOT FILTERED TO MATCH N YET!
(fig_seasonal3 <- ggplot(q_monthly_peaks %>%
                             left_join(.,domains),
                         aes(x = peak_month_q, fill = domain)) +
        scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
        scale_fill_viridis(discrete = TRUE) +
        geom_bar(position = "stack") +
        labs(x = "Peak Month", y = "Site Count",
             fill = "Domain") +
        theme_bw())

# ggsave(fig_seasonal3,
#        filename = "figures/q_monthly_peaks.jpg",
#        width = 20,
#        height = 10,
#        units = "cm")

# And join with N data to examine coincidence of peaks.
monthly_peak_match <- left_join(n_monthly_means, q_monthly_peaks, by = c("site_code")) %>%
    select(site_code, domain, var,
           peak_month, peak_season,
           peak_month_q, peak_season_q) %>%
    distinct() %>%
    mutate(peak_match = case_when(peak_season == peak_season_q ~ 1,
                                  TRUE ~ 0))

# End of script.
