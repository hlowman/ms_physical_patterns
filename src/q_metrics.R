# MacroSheds Discharge Metrics
# August 19, 2024
# Heili Lowman

#### READ ME ####

# The following script will calculate the "magnificent 7"
# discharge metrics (Archfield et al., 2014) as well as
# the Richards-Baker Flashiness Index (Baker et al., 2004)
# and some other annual temperature/climate metrics
# for all MacroSheds sites at which data is currently available.
library(here)
source(here('src', 'setup.R'))

# Load dataset - be patient, takes just a moment.
q_data <- ms_load_product(
  macrosheds_root = here(my_ms_dir),
  prodname = "discharge",
  warn = F
  )

#### Tidy ####

# Filter out repeat measures detected (n = 3,741 or ~ 0.1%).
q_data_nodup <- dplyr::distinct(q_data, site_code, datetime, .keep_all = TRUE)

# And filter out sites that were interpolated (n = 490,424 or ~ 25%).
q_data_nodup <- q_data_nodup %>%
  filter(ms_interp == 0)

# Only ~ 2% of records were marked as "1" or "questionable"
# in the ms_status column, so we've left that as is.

# Normalize by watershed area.
area <- ms_site_data %>%
  select(site_code, ws_area_ha)

# And convert to mm/d.
# --- Conversion equation ---
# (L/s*ha) (1,000 m3s/Lps) * (86,400 s/d) * (1/10,000 ha/m2) * (1/1,000 mm/m) = 8.64 mm/d*ha
q_data_nodup <- left_join(q_data_nodup, area, by = c("site_code")) %>%
  mutate(val_mmd = (val*86400*10000)/(ws_area_ha*100000000))

#### Water Years ####

# Assign a consistent water year designation to all data,
# from October 1 of the previous year to September 30 of
# the following (i.e., WY2022 is 10/01/2021 - 09/30/2022).
q_data_nodup <- q_data_nodup %>%
  mutate(month = month(datetime),
         year = year(datetime)) %>%
  mutate(water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                TRUE ~ year))

#### Filter to complete years ####
# will need to run q_good_data script first, uncomment next line to make the rds needed
#source(here('src', 'q_good_data.R'))
good_site_years <- readRDS(here('data_working', 'good_site_years.RDS'))

q_data_nodup <- q_data_nodup %>%
    right_join(., good_site_years, by = c('site_code')) %>%
    filter(water_year > start,
           water_year < end)

#### Q Metrics ####

##### AR(1) #####

# A few additional calculations are required for the
# auto-regressive (AR(1)) correlation calculation.

# Calculate long-term monthly means.
q_data_monthly <- q_data_nodup %>%
  group_by(site_code, month) %>%
  summarize(lt_meanQ = mean(val_mmd, na.rm = TRUE)) %>%
  ungroup()

# And use those values to calculate deseasonalized Q.
q_data_nodup <- q_data_nodup %>%
  plyr::join(q_data_monthly, by = c("site_code", "month")) %>%
  mutate(deseasonQ = val_mmd - lt_meanQ)

# And scale those values to use in the AR(1) calculations.
q_data_nodup$scaleQds = (q_data_nodup$deseasonQ - mean(q_data_nodup$deseasonQ,
                                                       na.rm = TRUE))/
  sd(q_data_nodup$deseasonQ, na.rm = TRUE)

# Function to pull out AR(1) correlation coefficient
ar1_print <- function(x) {
  a1 <- ar(x, aic = FALSE, order.max = 1, na.action = na.pass)
  a1$ar
}

##### Amplitude/Phase #####

# Also, need to solve for streamflow signal at each site
# using scaled (but not deseasonalized) discharge data
# as well as decimal year.
q_data_nodup <- q_data_nodup %>%
  mutate(scaleQ = (q_data_nodup$val_mmd -
                     mean(q_data_nodup$val_mmd, na.rm = TRUE))/
           sd(q_data_nodup$val_mmd, na.rm = TRUE),
         decimalY = decimal_date(datetime))

# Add covariates for streamflow signal regression.
q_data_nodup <- q_data_nodup %>%
  mutate(sin_2pi_year = sin(2*pi*decimalY),
         cos_2pi_year = cos(2*pi*decimalY))

##### RBI #####

# Function to calculate the Richard-Baker flashiness index.
rbi_print <- function(x) {
    # Checked to be sure that when grouped, the "diff" function
    # omits the first record for each site-water year.
    d <- diff(x)
    RBI <- sum(abs(d))/sum(x[2:length(x)])
    return(RBI)
}

##### Median Cumulative Q #####

# Calculate the day of year when the median
# cumulative discharge is reached.
q_data_50_doy <- q_data_nodup %>%
    group_by(site_code, water_year) %>%
        mutate(q50_sum = 0.5*sum(val_mmd),
               q_sum = cumsum(val_mmd)) %>%
        mutate(q50_exceed = case_when(q_sum > q50_sum ~ 1,
                                      q_sum <= q50_sum ~ 0,
                                      TRUE ~ NA)) %>%
    ungroup() %>%
    filter(q50_exceed == 1) %>%
    group_by(site_code, water_year) %>%
    slice_head() %>%
    # reformat date to be dys into the WY
    mutate(q50_dowy_exceed = as.numeric(difftime(datetime,
                                                 as_date(paste(water_year-1, "10", "01")),
                                                 units = "days"))) %>%
    # and keep only columns of interest for later joining
    rename(q50_date_exceed = datetime) %>%
    select(site_code, water_year, q50_sum, q50_date_exceed, q50_dowy_exceed)

# Create summarized dataset with all 8 metrics by site-water year.
q_metrics_siteyear <- q_data_nodup %>%
  # drop all NA discharge values
  # otherwise the lm() with full missing WYs below will throw
  # an error message and not output the summarized data
  drop_na(val_mmd) %>%
  # also dropping site-water years that broke the regressions' code
  mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
  #full_join(q_wy_counts) %>%
  #full_join(q_wy_mean) %>%
  #filter(use == 1) %>%
  #filter(use2 == 1) %>%
  # finally, calculate the discharge metrics
  group_by(site_code, water_year) %>%
  summarize(m1_meanq = mean(val_mmd, na.rm = TRUE), # mean
            q1 = quantile(val_mmd, probs = 0.01, na.rm = TRUE), # 1st percentile Q
            q5 = quantile(val_mmd, probs = 0.05, na.rm = TRUE), # 5th percentile Q
            q25 = quantile(val_mmd, probs = 0.25, na.rm = TRUE), # 25th percentile Q
            q50 = quantile(val_mmd, probs = 0.50, na.rm = TRUE), # median Q
            q75 = quantile(val_mmd, probs = 0.75, na.rm = TRUE), # 75th percentile Q
            q95 = quantile(val_mmd, probs = 0.95, na.rm = TRUE), # 95th percentile Q
            q99 = quantile(val_mmd, probs = 0.99, na.rm = TRUE), # 99th percentile Q
            m2_cvq = (sd(val_mmd, na.rm = TRUE)/
                        mean(val_mmd, na.rm = TRUE)), # coefficient of variation
            m3_skewq = skewness(val_mmd, na.rm = TRUE), # skewness
            m4_kurtq = kurtosis(val_mmd, na.rm = TRUE), # kurtosis
            m5_ar1q = ar1_print(scaleQds), # AR(1) coefficient
            rbiq = rbi_print(val_mmd), # Richards-Baker flashiness index
            # seasonal flow signal metrics a + b
            # per p. 1169 in Archfield et al., 2014
            a_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["sin_2pi_year"],
            b_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["cos_2pi_year"]) %>%
  ungroup() %>%
  mutate(m6_ampq = sqrt((a_flow_sig)^2 + (b_flow_sig)^2), # amplitude
         m7_phiq = atan(-a_flow_sig/b_flow_sig)) # phase shift

# Join with days on which 50% of cumulative flow is exceeded.
q_metrics_siteyear <- full_join(q_metrics_siteyear, q_data_50_doy)

#### Climate Metrics ####

# join in climate data
### this next one will take a minute
# only need to run it once!
clim <- read_feather(here('data_raw', 'spatial_timeseries_climate.feather')) %>%
  mutate(year = year(date),
         month = month(date),
        water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date)) %>%
    mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
                              month %in% c(12,1,2) ~ "Winter",
                              TRUE ~ NA))

##### Winter Min #####

clim_Wmin <- clim %>%
    filter(season == "Winter") %>%
    group_by(site_code, water_year) %>%
    drop_na(cc_temp_mean_median) %>%
    summarize(mintemp_winter = min(cc_temp_mean_median)) %>%
    ungroup()

##### Summer Mean #####

clim_Smean <- clim %>%
    filter(season == "Summer") %>%
    group_by(site_code, water_year) %>%
    drop_na(cc_temp_mean_median) %>%
    summarize(meantemp_summer = min(cc_temp_mean_median)) %>%
    ungroup()

##### Median Cumulative P #####
# Calculate the day of year when the median
# precip is reached.
clim_50_doy <- clim %>%
    group_by(site_code, water_year) %>%
    mutate(p50_sum = 0.5*sum(cc_precip_median, na.rm = T),
           p_sum = cumsum(cc_precip_median)) %>%
    mutate(p50_exceed = case_when(p_sum > p50_sum ~ 1,
                                  p_sum <= p50_sum ~ 0,
                                  TRUE ~ NA)) %>%
    ungroup() %>%
    filter(p50_exceed == 1) %>%
    group_by(site_code, water_year) %>%
    slice_head() %>%
    # reformat date to be dys into the WY
    mutate(p50_dowy_exceed = as.numeric(difftime(date,
                                                 as_date(paste(water_year-1, "10", "01")),
                                                 units = "days"))) %>%
    # and keep only columns of interest for later joining
    rename(p50_date_exceed = date) %>%
    select(site_code, water_year, p50_sum, p50_date_exceed, p50_dowy_exceed)

clim_metrics_siteyear <- clim %>%
    group_by(site_code, water_year) %>%
    summarize(precip_mean_ann = mean(cc_precip_median, na.rm = T),
              precip_total_ann = sum(cc_precip_median, na.rm = T),
              temp_mean_ann = mean(cc_temp_mean_median, na.rm = T))

clim_metrics_siteyear <- left_join(clim_metrics_siteyear, clim_Wmin)
clim_metrics_siteyear <- left_join(clim_metrics_siteyear, clim_Smean)
clim_metrics_siteyear <- left_join(clim_metrics_siteyear, clim_50_doy)

saveRDS(clim_metrics_siteyear, file = here('data_working', 'clim_summaries.rds'))

clim <- readRDS(here('data_working', 'clim_summaries.rds'))

q_metrics_siteyear %>%
    left_join(., clim, by = c('site_code', 'water_year')) %>%
    mutate(runoff_ratio = m1_meanq/precip_mean_ann) %>%
# Export data.
saveRDS(., "data_working/discharge_metrics_siteyear.rds")

# Create summarized dataset with all 8 metrics for full time series at each site.
q_metrics_site <- q_data_nodup %>%
  group_by(site_code) %>%
  # drop all NA discharge values
  # otherwise the lm() will throw an error message
  drop_na(val_mmd) %>%
  # also dropping site-water years that broke the regressions' code previously
  mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
  full_join(q_wy_mean) %>%
  filter(use == 1) %>%
  filter(use2 == 1) %>%
  summarize(m1_meanq = mean(val_mmd, na.rm = TRUE), # mean
            q1 = quantile(val_mmd, probs = 0.01, na.rm = TRUE), # 1st percentile Q
            q5 = quantile(val_mmd, probs = 0.05, na.rm = TRUE), # 5th percentile Q
            q25 = quantile(val_mmd, probs = 0.25, na.rm = TRUE), # 25th percentile Q
            q50 = quantile(val_mmd, probs = 0.50, na.rm = TRUE), # median Q
            q75 = quantile(val_mmd, probs = 0.75, na.rm = TRUE), # 75th percentile Q
            q95 = quantile(val_mmd, probs = 0.95, na.rm = TRUE), # 95th percentile Q
            q99 = quantile(val_mmd, probs = 0.99, na.rm = TRUE), # 99th percentile Q
            m2_cvq = (sd(val_mmd, na.rm = TRUE)/
                        mean(val_mmd, na.rm = TRUE)), # coefficient of variation
            m3_skewq = skewness(val_mmd, na.rm = TRUE), # skewness
            m4_kurtq = kurtosis(val_mmd, na.rm = TRUE), # kurtosis
            m5_ar1q = ar1_print(scaleQds), # AR(1) coefficient
            rbiq = rbi_print(val_mmd), # Richards-Baker flashiness index
            a_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["sin_2pi_year"],
            b_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["cos_2pi_year"]) %>% # flow signal metrics
  ungroup() %>%
  mutate(m6_ampq = sqrt((a_flow_sig)^2 + (b_flow_sig)^2), # amplitude
         m7_phiq = atan(-a_flow_sig/b_flow_sig)) # phase shift

# Export data.
saveRDS(q_metrics_site, "data_working/discharge_metrics.rds")

# End of script.
