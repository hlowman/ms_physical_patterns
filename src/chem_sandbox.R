# Chemistry Trends Sandbox Script
# April 8, 2025

# The following script will be used to test and deploy functions
# to both trim and estimate trends for all stream chemistry data.

# Load necessary packages.
library(here)
source(here('src', 'setup.R'))

# Load VWM nitrogen datasets.
n_q_vwm_month <- readRDS("data_working/N_VWM_monthly.rds")
n_q_vwm_seas <- readRDS("data_working/N_VWM_seasonal.rds")
n_q_vwm_ann <- readRDS("data_working/N_VWM_annual.rds")

# Tidy column names.
n_q_vwm_month <- n_q_vwm_month %>%
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

n_q_vwm_seas <- n_q_vwm_seas %>%
    rename(analyte = analyte_N,
           water_year = season_year,
           timestep = season,
           vwm_mgL = seasonal_vwm_mgL,
           vwm_mgLha = seasonal_vwm_mgLha)

n_q_vwm_ann <- n_q_vwm_ann %>%
    rename(analyte = analyte_N,
           vwm_mgL = annual_vwm_mgL,
           vwm_mgLha = annual_vwm_mgLha) %>%
    mutate(timestep = "Annual")

# Join all VWM nitrogen data together.
n_q_vwm <- full_join(n_q_vwm_ann, n_q_vwm_month)
n_q_vwm <- full_join(n_q_vwm, n_q_vwm_seas)

# Testing function.
no3_data <- gen_reduce_to_best_range(data_in = n_q_vwm,
                                     solute = 'NO3_N',
                                     aggregation = 'Annual')

no3_data_paired <- gen_reduce_to_best_range(data_in = n_q_vwm,
                                     solute = 'NO3_N',
                                     pair_data = 'YES',
                                     aggregation = 'Annual')

# End of script.
