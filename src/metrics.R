# READ ME ####
# The following script will calculate the "magnificent 7"
# discharge metrics (Archfield et al., 2014) as well as
# the Richards-Baker Flashiness Index (Baker et al., 2004)
# and some other annual temperature/climate metrics
# for all MacroSheds sites at which data is currently available.
library(here)
source(here('src', 'setup.R'))

# set logging
set_logger()
# Q ####
## read data ####
log_info('load data')

q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F
)

log_info({nrow(q_data)}, ' rows of discharge data')

## Tidy ####

# Filter out repeat measures detected (n = 3,741 or ~ 0.1%).
q_data_nodup <- dplyr::distinct(q_data, site_code,
                                date, .keep_all = TRUE)

log_info({nrow(q_data) - nrow(q_data_nodup)}, ' rows of discharge data removed during duplicate check')

# And filter out sites that were interpolated (n = 490,424 or ~ 25%).
q_data_nointerp <- q_data_nodup %>%
  filter(ms_interp == 0)

log_info({nrow(q_data_nodup) - nrow(q_data_nointerp)},
         ' rows of discharge data removed during interp check')

sites_lost <- q_data_nodup %>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(q_data_nointerp$site_code))

if(length(sites_lost > 0)){
log_warn({nrow(sites_lost)}, ' sites lost')
}


# Only ~ 2% of records were marked as "1" or "questionable"
# in the ms_status column, so we've left that as is.
log_info('normalize q to watershed area')
# Normalize by watershed area.
area <- ms_site_data %>%
  select(site_code, ws_area_ha)

# And convert to mm/d.
# --- Conversion equation ---
# (L/s*ha) (1,000 m3s/Lps) * (86,400 s/d) * (1/10,000 ha/m2) * (1/1,000 mm/m) = 8.64 mm/d*ha
q_data_nointerp_scaled <- left_join(q_data_nointerp, area,
                                    by = c("site_code")) %>%
  mutate(val_mmd = (val*86400*10000)/(ws_area_ha*100000000))

## Water Years ####

# Assign a consistent water year designation to all data,
# from October 1 of the previous year to September 30 of
# the following (i.e., WY2022 is 10/01/2021 - 09/30/2022).
log_info('assigning water years')

q_data_scaled <- q_data_nointerp_scaled %>%
  mutate(month = month(date),
         year = year(date)) %>%
  mutate(water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                TRUE ~ year))

## q freq check ####
log_info('performing freq check')
freq_check <- frequency_check(q_data_scaled)

write_csv(freq_check, here('data_working', 'all_possible_good_siteyears.csv'))
log_info('all_possible_good_siteyears.csv has been created')

q_data_good <- q_data_scaled %>%
    right_join(., freq_check, by = c('site_code', 'water_year'))


log_info({nrow(q_data_scaled) - nrow(q_data_good)},
         ' rows of discharge data removed during freq check')

sites_lost <- q_data_scaled %>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(q_data_good$site_code))

if(length(sites_lost > 0)){
    log_warn({nrow(sites_lost)}, ' sites lost in q freq check')
}

## AR(1) ####

# A few additional calculations are required for the
# auto-regressive (AR(1)) correlation calculation.
log_info('seasonal extraction')
# Calculate long-term monthly means.
q_data_monthly <- q_data_good %>%
  group_by(site_code, month) %>%
  summarize(lt_meanQ = mean(val_mmd, na.rm = TRUE)) %>%
  ungroup()

# And use those values to calculate deseasonalized Q.
q_data_good <- q_data_good %>%
  plyr::join(q_data_monthly, by = c("site_code", "month")) %>%
  mutate(deseasonQ = val_mmd - lt_meanQ)

# And scale those values to use in the AR(1) calculations.
q_data_good$scaleQds = (q_data_good$deseasonQ - mean(q_data_good$deseasonQ,
                                                       na.rm = TRUE))/
  sd(q_data_good$deseasonQ, na.rm = TRUE)

# Function to pull out AR(1) correlation coefficient
ar1_print <- function(x) {
  a1 <- ar(x, aic = FALSE, order.max = 1, na.action = na.pass)
  a1$ar
}

## Amplitude/Phase #####
log_info('amplitude and phase extraction')
# Also, need to solve for streamflow signal at each site
# using scaled (but not deseasonalized) discharge data
# as well as decimal year.
q_data_good <- q_data_good %>%
  mutate(scaleQ = (q_data_good$val_mmd -
                     mean(q_data_good$val_mmd, na.rm = TRUE))/
           sd(q_data_good$val_mmd, na.rm = TRUE),
         decimalY = decimal_date(date),
# Add covariates for streamflow signal regression.
        sin_2pi_year = sin(2*pi*decimalY),
         cos_2pi_year = cos(2*pi*decimalY))

## RBI #####

# Function to calculate the Richard-Baker flashiness index.
rbi_print <- function(x) {
    # Checked to be sure that when grouped, the "diff" function
    # omits the first record for each site-water year.
    d <- diff(x)
    RBI <- sum(abs(d))/sum(x[2:length(x)])
    return(RBI)
}

## Median Cumulative Q #####
log_info('median cumlative q')
# Calculate the day of year when the median
# cumulative discharge is reached.
q_data_50_doy <- q_data_good %>%
    group_by(site_code, water_year) %>%
        mutate(q50_sum = 0.5*sum(val_mmd),
               q_sum = cumsum(val_mmd),
        q50_exceed = case_when(q_sum > q50_sum ~ 1,
                                      q_sum <= q50_sum ~ 0,
                                      TRUE ~ NA)) %>%
    ungroup() %>%
    filter(q50_exceed == 1) %>%
    group_by(site_code, water_year) %>%
    slice_head() %>%
    # reformat date to be dys into the WY
    mutate(q50_dowy_exceed = as.numeric(difftime(date,
                                                 as_date(paste(water_year-1, "10", "01")),
                                                 units = "days"))) %>%
    # and keep only columns of interest for later joining
    rename(q50_date_exceed = date) %>%
    select(site_code, water_year, q50_sum, q50_date_exceed, q50_dowy_exceed)

# Calculate number of records for each site-water year,
# since those with too few records will break the
# regressions below.
log_info('generating watershed site_year counts')
q_wy_counts <- q_data_good %>%
    drop_na(val_mmd) %>%
    mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
    count(site_wy) %>%
    ungroup() %>%
    mutate(use = case_when(n > 3 ~ 1,
                           n <= 3 ~ 0,
                           TRUE ~ NA)) %>%
    select(site_wy, use)

# Also notate sites for which the site-water year mean
# discharge is zero, which will also not work with the
# summary calculations below.
q_wy_mean <- q_data_good %>%
    drop_na(val_mmd) %>%
    mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
    group_by(site_wy) %>%
    summarize(mean = mean(val_mmd, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(use2 = case_when(mean > 0 ~ 1,
                            mean <= 0 ~ 0,
                            TRUE ~ NA)) %>%
    select(site_wy, use2)

log_info('make q metrics output frame')
# Create summarized dataset with all 8 metrics by site-water year.
q_metrics_siteyear <- q_data_good %>%
  # drop all NA discharge values
  # otherwise the lm() with full missing WYs below will throw
  # an error message and not output the summarized data
  drop_na(val_mmd) %>%
  mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
  # also dropping site-water years that broke the regressions' code
  full_join(q_wy_counts) %>%
  full_join(q_wy_mean) %>%
  filter(use == 1) %>%
  filter(use2 == 1) %>%
  # finally, calculate the discharge metrics
  group_by(site_code, water_year) %>%
  summarize(q_mean = mean(val_mmd, na.rm = TRUE), # mean
            q_q1 = quantile(val_mmd, probs = 0.01, na.rm = TRUE), # 1st percentile Q
            q_q5 = quantile(val_mmd, probs = 0.05, na.rm = TRUE), # 5th percentile Q
            q_q25 = quantile(val_mmd, probs = 0.25, na.rm = TRUE), # 25th percentile Q
            q_q50 = quantile(val_mmd, probs = 0.50, na.rm = TRUE), # median Q
            q_q75 = quantile(val_mmd, probs = 0.75, na.rm = TRUE), # 75th percentile Q
            q_q95 = quantile(val_mmd, probs = 0.95, na.rm = TRUE), # 95th percentile Q
            q_q99 = quantile(val_mmd, probs = 0.99, na.rm = TRUE), # 99th percentile Q
            q_cv = (sd(val_mmd, na.rm = TRUE)/
                        mean(val_mmd, na.rm = TRUE)), # coefficient of variation
            q_skew = skewness(val_mmd, na.rm = TRUE), # skewness
            q_kurt = kurtosis(val_mmd, na.rm = TRUE), # kurtosis
            q_ar1 = ar1_print(scaleQds), # AR(1) coefficient
            q_rbi = rbi_print(val_mmd), # Richards-Baker flashiness index
            # seasonal flow signal metrics a + b
            # per p. 1169 in Archfield et al., 2014
            a_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["sin_2pi_year"],
            b_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["cos_2pi_year"]) %>%
  ungroup() %>%
  mutate(q_amp = sqrt((a_flow_sig)^2 + (b_flow_sig)^2), # amplitude
         q_phi = atan(-a_flow_sig/b_flow_sig)) # phase shift

sites_lost <- q_data_good%>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(q_metrics_siteyear$site_code))

if(length(sites_lost > 0)){
    log_warn({nrow(sites_lost)}, ' sites lost in metric computation')
}else{log_info('no sites lost in q metric computation')}

# Join with days on which 50% of cumulative flow is exceeded.
q_metrics_siteyear <- full_join(q_metrics_siteyear, q_data_50_doy)

# CLIMATE #####
# read in climate data
log_info('read climate data')
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

## Winter Min #####
log_info('calculate winter min')
clim_Wmin <- clim %>%
    filter(season == "Winter") %>%
    group_by(site_code, water_year) %>%
    drop_na(temp_median) %>%
    summarize(min_air_temp_winter = min(temp_median)) %>%
    ungroup()

## Summer Mean #####
log_info('calculate summer mean')
clim_Smean <- clim %>%
    filter(season == "Summer") %>%
    group_by(site_code, water_year) %>%
    drop_na(temp_median) %>%
    summarize(mean_air_temp_summer = mean(temp_median)) %>%
    ungroup()

## Median Cumulative P #####
# Calculate the day of year when the median
# precip is reached.
log_info('calculate median cum p')
clim_50_doy <- clim %>%
    group_by(site_code, water_year) %>%
    mutate(p50_sum = 0.5*sum(precip_median, na.rm = T),
           p_sum = cumsum(precip_median)) %>%
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

# join together
clim_metrics_siteyear <- clim %>%
    group_by(site_code, water_year) %>%
    summarize(precip_mean_ann = mean(precip_median, na.rm = T),
              precip_total_ann = sum(precip_median, na.rm = T),
              temp_mean_ann = mean(temp_median, na.rm = T)) %>%
    left_join(., clim_Wmin) %>%
    left_join(.,clim_Smean) %>%
    left_join(., clim_50_doy)

# STREAM TEMP ####
# read data ####
t_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "stream_chemistry",
    filter_vars = 'temp',
    warn = F) %>%
    mutate(month = month(date),
           year = year(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year))

log_info({nrow(t_data)}, ' rows of stream temp data')
# want at least monthly sampling for most of the year for now
# 51/52 weeks of the year
log_info('performing freq check on stream temp')

freq_check <- t_data %>%
            filter(ms_interp == 0) %>%
            mutate(month_year = paste0(month(date), '_', water_year)) %>%
    group_by(site_code, ,month_year) %>%
    summarize(water_year = max(water_year),
              n = n()) %>%
    filter(n >= 1) %>%
    group_by(site_code, water_year) %>%
    summarize(n = n()) %>%
    filter(n >= 10)

t_good <- t_data %>%
    filter(ms_interp == 0) %>%
    mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
                              month %in% c(12,1,2) ~ "Winter",
                              month %in% c(3,4,5) ~ "Spring",
                              month %in% c(9,10,11) ~ "Fall",
                              TRUE ~ NA)) %>%
    right_join(., freq_check, by = c('site_code', 'water_year')) %>%
    select(site_code, water_year, season, var, val) %>%
    na.omit()

t_ann <- t_good %>%
    group_by(site_code, water_year) %>%
    summarize(stream_temp_mean_ann = mean(val, na.rm = T))

log_info({nrow(t_data) - nrow(t_good)}, ' rows of stream temp data removed during freq/interp check')

sites_lost <- t_data %>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(t_good$site_code))

if(length(sites_lost > 0)){
    log_warn({nrow(sites_lost)}, ' sites lost in stream temp freq/interp check')
}

## Winter Min #####
log_info('calculate winter min stream temp')
t_wmin <- t_good %>%
    filter(season == "Winter") %>%
    group_by(site_code, water_year) %>%
    summarize(stream_temp_min_winter = min(val, na.rm = T)) %>%
    ungroup()

## Summer Mean #####
log_info('calculate summer mean stream temp')
t_smean <- t_good %>%
    filter(season == "Summer") %>%
    group_by(site_code, water_year) %>%
    summarize(stream_temp_mean_summer = mean(val, na.rm = T)) %>%
    ungroup()

t_out <- t_ann %>%
    full_join(., t_wmin, by = c('site_code', 'water_year')) %>%
    full_join(., t_smean, by = c('site_code', 'water_year'))

# STREAM CHEMISTRY ####

# Note - this only works up nitrogen for now, but will likely
# add DOC in here in the future.

# read data ####
chem_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "stream_chemistry",
    filter_vars = c("NH4_N", "NO3_NO2_N", "TDN",
                    "TPN", "NO3_N", "TN",
                    "TIN", "NO2_N", "NH3_N",
                    "TDKN", "TKN", "N2O",
                    "NO3_NO2_N", "NH3_NH4_N"), warn = F) %>%
    # remove interpolated values
    filter(ms_interp == 0)

# note - ms_calc_vwc has been deprecated from the most recent
# version of the macrosheds package
# so, hard-coding the calculation of volume-weighted mean
# monthly concentrations below

# Now, join with q data.
q_data_scaled$q_Lsecha <- q_data_scaled$val/q_data_scaled$ws_area_ha

q_trim <- q_data_scaled %>%
    select(date, site_code, ws_area_ha, q_Lsecha)

chem_q_data <- left_join(chem_data, q_trim)

# And convert to volume-weight mean concentrations,
# on a monthly basis.
chem_q_vwm <- chem_q_data %>%
    # Create temporal columns for later grouping.
    mutate(year = year(date),
           month = month(date),
           day = day(date)) %>%
    # Calculate instantaneous C*Q.
    mutate(c_q_instant = val*q_Lsecha) %>%
    # Now impose groupings.
    group_by(site_code, var, year, month) %>%
    # Calculate mean monthly volume weighted concentrations.
    summarize(monthly_vwm_mgL = (sum(c_q_instant))/(sum(q_Lsecha)),
              n_days_of_obs_chem = n()) %>%
    ungroup() %>%
    # Add water year
    mutate(water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                               TRUE ~ year)) %>%
    # and remove remaining NAs
    na.omit()

log_info({nrow(chem_q_vwm)}, ' rows of stream vwm chemistry data')
# want at least biweekly sampling for 10 months of the year
log_info('performing freq check on stream vwm chemistry')

freq_check_chem <- chem_q_vwm %>%
    mutate(site_wyear = paste0(site_code, '_', water_year)) %>%
    # 2,971 site-years remaining at 184 sites (note, across all analytes)
    # minimum 2 samples per month
    filter(n_days_of_obs_chem >= 2) %>%
    # 2,855 site-years remaining at 182 sites (again across all analytes)
    # This drops to 2,199 site-years at 164 sites if we bump to 4 samples/month
    group_by(site_code, water_year, site_wyear, var) %>%
    summarize(n = n()) %>%
    # minimum 10 months per year
    filter(n >= 10)
    # now 1,197 site-years remaining at 106 sites

chem_q_good <- chem_q_vwm %>%
    mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
                              month %in% c(12,1,2) ~ "Winter",
                              month %in% c(3,4,5) ~ "Spring",
                              month %in% c(9,10,11) ~ "Fall",
                              TRUE ~ NA)) %>%
    right_join(., freq_check_chem,
               by = c('site_code',
                      'var',
                      'water_year')) %>%
    na.omit()

log_info({nrow(chem_q_vwm) - nrow(chem_q_good)}, ' rows of stream vwm chem data removed during freq check')

sites_lost <- chem_q_vwm %>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(chem_q_good$site_code))

if(length(sites_lost > 0)){
    log_warn({nrow(sites_lost)}, ' sites lost in stream vwm chem freq check')
}

# Commenting out flux calculations for now since we're using VWMs.
# q_monthly <- q_data_scaled %>%
#      # Now impose groupings.
#     group_by(site_code, year, month) %>%
#     # Calculate monthly discharge.
#     summarize(monthly_mean_q_Ldh = mean(q_Lsecha)*60*60*24, # daily mean discharge
#               monthly_sum_q_Lh = sum(q_Lsecha)*60*60*24, # monthly cumulative discharge
#               n_days_of_obs_q = n()) %>%
#     ungroup()

# chem_q_good <- left_join(chem_q_good, q_monthly) %>%
#     mutate(monthly_mean_flux_kgdh = (monthly_vwm_mgL*monthly_mean_q_Ldh)/1000,
#            monthly_sum_flux_kgh = (monthly_vwm_mgL*monthly_sum_q_Lh)/1000)

## Monthly means #####
log_info('widen dataset for trend analysis')
n_monthly_vwmeans <- chem_q_good %>%
    select(site_code, var, water_year, month, monthly_vwm_mgL) %>%
    pivot_wider(
        names_from = c(var, month),
        values_from = monthly_vwm_mgL)

## Annual means #####
log_info('also append annual means for trend analysis')
n_annual_vwmeans <- chem_q_good %>%
    group_by(site_code, var, water_year) %>%
    summarize(Annual = mean(monthly_vwm_mgL, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(
        names_from = var,
        values_from = Annual) %>%
    rename(NH4_N_Annual = NH4_N,
           NO3_NO2_N_Annual = NO3_NO2_N,
           TDN_Annual = TDN,
           TPN_Annual = TPN,
           NO3_N_Annual = NO3_N,
           TIN_Annual = TIN,
           TN_Annual = TN,
           NO2_N_Annual = NO2_N,
           NH3_N_Annual = NH3_N,
           TDKN_Annual = TDKN,
           TKN_Annual = TKN,
           N2O_Annual = N2O)

n_vwmeans <- full_join(n_monthly_vwmeans, n_annual_vwmeans)

# Quick plots to make sure these appear to populate correctly.
# ggplot(n_vwmeans %>%
#            select(site_code:NH4_N_11) %>%
#            filter(site_code == "w6") %>%
#            pivot_longer(cols = NH4_N_12:NH4_N_11, names_to = "var_month"),
#        aes(x = water_year, y = value)) +
#     geom_point() +
#     geom_line() +
#     theme_bw() +
#     theme(legend.position = "none") +
#     facet_wrap(.~var_month, scales = "free")

# PRODUCTIVITY ####
## read data ####
p_data <- read_feather(here('data_raw', 'spatial_timeseries_vegetation.feather')) %>%
    mutate(month = month(date),
           year = year(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year))

log_info({nrow(p_data)}, ' rows of productivity data')


p_out <- p_data %>%
    distinct() %>%
    mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
                              month %in% c(12,1,2) ~ "Winter",
                              month %in% c(3,4,5) ~ "Spring",
                              month %in% c(9,10,11) ~ "Fall",
                              TRUE ~ NA)) %>%
    group_by(site_code, water_year, var) %>%
    summarize(val = mean(val, na.rm = T)) %>%
    pivot_wider(id_cols = c('site_code', 'water_year'),
                names_from = 'var', values_from = 'val') %>%
    select(site_code, water_year, lai = lai_median,
           ndvi = ndvi_median, tree_cover = tree_cover_median,
           veg_cover = veg_cover_median,
           evi = evi_median, lai = lai_median,
           gpp_conus = gpp_CONUS_30m_median,
           gpp_global = gpp_global_500m_median)

# EXPORT ####
## climate ####
out_path <- here('data_working', 'clim_summaries.rds')
saveRDS(clim_metrics_siteyear, file = out_path)
log_info('file saved to ', out_path)
## site_year level ####
log_info('saving out')
q_data_out <- q_metrics_siteyear %>%
    full_join(., readRDS(here('data_working', 'clim_summaries.rds')), by = c('site_code', 'water_year')) %>%
    mutate(runoff_ratio = q_mean/precip_mean_ann) %>%
    full_join(., t_out, by = c('site_code', 'water_year')) %>%
    full_join(., p_out, by = c('site_code', 'water_year')) %>%
    full_join(., n_vwmeans, by = c('site_code', 'water_year')) %>%
    distinct()

out_path <- here('data_working', 'discharge_metrics_siteyear.rds')
saveRDS(q_data_out, out_path)
log_info('file saved to ', out_path)

# Create summarized dataset with all 8 metrics for full time series at each site.
log_info('calculate site level metrics')
q_metrics_site <- q_data_good %>%
  group_by(site_code) %>%
  # drop all NA discharge values
  # otherwise the lm() will throw an error message
  drop_na(val_mmd) %>%
  # also dropping site-water years that broke the regressions' code previously
  mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
  summarize(q_mean = mean(val_mmd, na.rm = TRUE), # mean
            q_q1 = quantile(val_mmd, probs = 0.01, na.rm = TRUE), # 1st percentile Q
            q_q5 = quantile(val_mmd, probs = 0.05, na.rm = TRUE), # 5th percentile Q
            q_q25 = quantile(val_mmd, probs = 0.25, na.rm = TRUE), # 25th percentile Q
            q_q50 = quantile(val_mmd, probs = 0.50, na.rm = TRUE), # median Q
            q_q75 = quantile(val_mmd, probs = 0.75, na.rm = TRUE), # 75th percentile Q
            q_q95 = quantile(val_mmd, probs = 0.95, na.rm = TRUE), # 95th percentile Q
            q_q99 = quantile(val_mmd, probs = 0.99, na.rm = TRUE), # 99th percentile Q
            q_cv = (sd(val_mmd, na.rm = TRUE)/
                        mean(val_mmd, na.rm = TRUE)), # coefficient of variation
            q_skew = skewness(val_mmd, na.rm = TRUE), # skewness
            q_kurt = kurtosis(val_mmd, na.rm = TRUE), # kurtosis
            q_ar1 = ar1_print(scaleQds), # AR(1) coefficient
            q_rbi = rbi_print(val_mmd), # Richards-Baker flashiness index
            a_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["sin_2pi_year"],
            b_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["cos_2pi_year"]) %>% # flow signal metrics
  ungroup() %>%
  mutate(q_amp = sqrt((a_flow_sig)^2 + (b_flow_sig)^2), # amplitude
         q_phi = atan(-a_flow_sig/b_flow_sig)) # phase shift

## site level ####
out_path <- here("data_working", "discharge_metrics_site.rds")
saveRDS(q_metrics_site, out_path)
log_info('file saved to ', out_path)
# End of script.
