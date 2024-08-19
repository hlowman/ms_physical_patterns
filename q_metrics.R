# MacroSheds Discharge Metrics
# August 19, 2024
# Heili Lowman

#### READ ME ####

# The following script will calculate the "magnificent 7"
# discharge metrics (Archfield et al., 2014) for all of 
# the MacroSheds sites at which data is currently available.

# Note, this script uses a newer version of the data sent by
# Mike on 8/19/24, not the current package version of the dataset.

#### Setup ####

# Load packages.
library(here)
library(tidyverse)
library(ggplot2)
library(naniar)
library(lubridate)
library(feather)
library(macrosheds)
library(moments)

# Using revised code from Mike to point to new data.
rdata_path <- "data_raw" # updated path

load(file.path(rdata_path, 'ms_site_data.RData'))
load(file.path(rdata_path, 'ms_vars_ws_attr.RData'))
load(file.path(rdata_path, 'ms_vars_ts.RData'))
load(file.path(rdata_path, 'ms_var_catalog.RData'))

nv <- as.environment('package:macrosheds')

for(ms_data in c('ms_vars_ts', 'ms_vars_ws', 'ms_site_data', 'ms_var_catalog')){
  unlockBinding(ms_data, nv)
  assign(ms_data, get(ms_data), envir = nv)
  lockBinding(ms_data, nv)
}

# Set directory to folder containing new data.
my_ms_dir <- "data_raw"

# Load dataset - be patient, takes just a moment.
q_data <- ms_load_product(
  macrosheds_root = here(my_ms_dir),
  prodname = "discharge"
  )

#### Water Years ####

# Filter out repeat measures detected at COMO/MCDI.
q_data_nodup <- dplyr::distinct(q_data, site_code, datetime, .keep_all = TRUE)

# Assign a consistent water year designation to all data,
# from October 1 of the previous year to September 30 of
# the following (i.e., WY2022 is 10/01/2021 - 09/30/2022).
q_data_nodup <- q_data_nodup %>%
  mutate(month = month(datetime),
         year = year(datetime)) %>%
  mutate(water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                TRUE ~ year))

#### 7 Metrics ####

##### AR(1) #####

# A few additional calculations are required for the
# auto-regressive (AR(1)) correlation calculation.

# Calculate long-term monthly means.
q_data_monthly <- q_data_nodup %>%
  group_by(site_code, month) %>%
  summarize(lt_meanQ = mean(val, na.rm = TRUE)) %>%
  ungroup()

# And use those values to calculate deseasonalized Q.
q_data_nodup <- q_data_nodup %>%
  plyr::join(q_data_monthly, by = c("site_code", "month")) %>%
  mutate(deseasonQ = val - lt_meanQ)

# And scale those values to use in the AR(1) calculations.
q_data_nodup$scaleQds = (q_data_nodup$deseasonQ - mean(q_data_nodup$deseasonQ, na.rm = TRUE))/
  sd(q_data_nodup$deseasonQ, na.rm = TRUE)

# Function to pull out AR(1) correlation coefficient
acf_print <- function(x) {
  a1 <- acf(x, lag.max = 1, plot = FALSE, na.action = na.pass)
  a1$acf[2]
}

##### Amplitude/Phase #####

# Also, need to solve for streamflow signal at each site
# using scaled (but not deseasonalized) discharge data
# as well as decimal year.
q_data_nodup <- q_data_nodup %>%
  mutate(scaleQ = (q_data_nodup$val - 
                     mean(q_data_nodup$val, na.rm = TRUE))/
           sd(q_data_nodup$val, na.rm = TRUE),
         decimalY = decimal_date(datetime))

# Add covariates for streamflow signal regression.
q_data_nodup <- q_data_nodup %>%
  mutate(sin_2pi_year = sin(2*pi*decimalY),
         cos_2pi_year = cos(2*pi*decimalY))

##### Summary #####

# Create summarized dataset with all 7 metrics.
q_metrics_siteyear <- q_data_nodup %>%
  # drop all NA discharge values
  # otherwise the lm() with full missing WYs below will throw
  # an error message and not output the summarized data
  drop_na(val) %>%
  group_by(site_code, water_year) %>%
  summarize(m1_meanq = mean(val, na.rm = TRUE), # mean
            m2_cvq = (sd(val, na.rm = TRUE)/
                        mean(val, na.rm = TRUE)), # coefficient of variation
            m3_skewq = skewness(val, na.rm = TRUE), # skewness
            m4_kurtq = kurtosis(val, na.rm = TRUE), # kurtosis
            m5_ar1q = acf_print(scaleQds), # AR(1) coefficient
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
# saveRDS(q_metrics_siteyear, "data_working/discharge_7metrics_siteyear_081924.rds")
  
# Then, summarize discharge metrics by site
# for easier plotting/visualization.

# Create summarized dataset with all 7 metrics.
q_metrics_site <- q_data_nodup %>%
  group_by(site_code) %>%
  summarize(m1_meanq = mean(val, na.rm = TRUE), # mean
            m2_cvq = (sd(val, na.rm = TRUE)/
                        mean(val, na.rm = TRUE)), # coefficient of variation
            m3_skewq = skewness(val, na.rm = TRUE), # skewness
            m4_kurtq = kurtosis(val, na.rm = TRUE), # kurtosis
            m5_ar1q = acf_print(scaleQds), # AR(1) coefficient
            a_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year, 
                            na.action = na.omit)$coefficients["sin_2pi_year"],
            b_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year, 
                            na.action = na.omit)$coefficients["cos_2pi_year"]) %>% # flow signal metrics
  ungroup() %>%
  mutate(m6_ampq = sqrt((a_flow_sig)^2 + (b_flow_sig)^2), # amplitude
         m7_phiq = atan(-a_flow_sig/b_flow_sig)) # phase shift

# Export data.
# saveRDS(q_metrics_site, "data_working/discharge_7metrics_site_081924.rds")

# End of script.
