# MacroSheds Discharge Metrics
# August 19, 2024
# Heili Lowman

#### READ ME ####

# The following script will calculate the "magnificent 7"
# discharge metrics (Archfield et al., 2014) as well as
# the Richards-Baker Flashiness Index (Baker et al., 2004)
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
# (L/s*ha) * (86,400 s/d) * (10e6 mm^3/L) * (10e-9 ha/mm^2) = 86.4 mm/d
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

#### Summary ####

# Calculate number of records for each site-water year,
# since those with too few records will break the
# regressions below.
q_wy_counts <- q_data_nodup %>%
    drop_na(val_mmd) %>%
    mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
    count(site_wy) %>%
    ungroup() %>%
    mutate(use = case_when(n > 3 ~ 1,
                           n <= 3 ~ 0,
                           TRUE ~ NA))
# ~75% of site-water years (n = 3,428/4,591) have 360+ days of discharge data

# Also notate sites for which the site-water year mean
# discharge is zero, which will also not work with the
# summary calculations below.
q_wy_mean <- q_data_nodup %>%
    drop_na(val_mmd) %>%
    mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
    group_by(site_wy) %>%
    summarize(mean = mean(val_mmd, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(use2 = case_when(mean > 0 ~ 1,
                           mean <= 0 ~ 0,
                           TRUE ~ NA))

# Create summarized dataset with all 8 metrics by site-water year.
q_metrics_siteyear <- q_data_nodup %>%
  # drop all NA discharge values
  # otherwise the lm() with full missing WYs below will throw
  # an error message and not output the summarized data
  drop_na(val_mmd) %>%
  # also dropping site-water years that broke the regressions' code
  mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
  full_join(q_wy_counts) %>%
  full_join(q_wy_mean) %>%
  filter(use == 1) %>%
  filter(use2 == 1) %>%
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

# Export data.
saveRDS(q_metrics_siteyear, "data_working/discharge_metrics_siteyear.rds")

# Create summarized dataset with all 8 metrics for full time series at each site.
q_metrics_site <- q_data_nodup %>%
  group_by(site_code) %>%
  # drop all NA discharge values
  # otherwise the lm() will throw an error message
  drop_na(val_mmd) %>%
  # also dropping site-water years that broke the regressions' code previously
  mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
  full_join(q_wy_counts) %>%
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
