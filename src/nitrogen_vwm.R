# Nitrogen data read-in script

#### README ####
# The following script will read in and prepare
# the nitrogen data for trend analyses. Primarily,
# it will convert concentrations to volume-weighted
# means. It will also make selections/combinations
# of the appropriate analytes wherever duplicates
# are present.

#### Load packages ####
library(here)
source(here('src', 'setup.R'))

#### Load data ####
# Stream Chemistry
chem_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "stream_chemistry",
    warn = F
)

# Discharge
q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F
)

#### Analyte filter ####

# First, we need to see what analytes are available and
# roughly in what frequency to be useful.
unique(chem_data$var)

# V1 codes:
# "GN_NH4_N", "GN_NO3_NO2_N", "GN_TDN",
# "GN_TPN", "GN_NO3_N", "GN_TN",
# "GN_d15N_NO3", "IN_DON", "IN_NO3_N",
# "IN_TDN", "GN_DON", "GN_TIN",
# "GN_NO2_N", "IN_NO2_N", "IN_TN",
# "GN_NH3_N", "GN_TDKN", "GN_TKN",
# "GN_N2O", "IS_NO3_N", "IN_NO3_NO2_N",
# "GN_NH3_NH4_N", "GN_TON"
# V1 code key:
# G = grab sample
# I = installed sampling (e.g., ISCO)
# N = not sensor
# S = sensor

# Not using TON/DON since they're calculated,
# but let's see the rough number of the remaining
# options.
n_counts <- chem_data %>%
    filter(var %in% c("NH4_N", "NO3_NO2_N", "TDN",
                      "TPN", "NO3_N", "TN",
                      "d15N_NO3", "TIN", "NO2_N",
                      "NH3_N", "TDKN", "TKN",
                      "N2O", "NH3_NH4_N")) %>% # down to ~7million records
    # remove NAs
    drop_na(val) %>% # down to ~2 million records
    # and remove interpolated values
    filter(ms_interp == 0) %>% # down to 400k records
    count(var) %>%
    ungroup()

# Also, we want to check to see what overlap there is in
# analytes within a given site.
analyte_counts <- chem_data %>%
    filter(var %in% c("NH4_N", "NO3_NO2_N", "TDN",
                      "TPN", "NO3_N", "TN",
                      "d15N_NO3", "TIN", "NO2_N",
                      "NH3_N", "TDKN", "TKN",
                      "N2O", "NH3_NH4_N")) %>% # down to ~7million records
    # remove NAs
    drop_na(val) %>% # down to ~2 million records
    # and remove interpolated values
    filter(ms_interp == 0) %>% # down to 400k records
    count(site_code, var) %>%
    ungroup() %>%
    pivot_wider(names_from = var, values_from = n) %>%
    select(site_code,
           NO3_N, NO2_N, NO3_NO2_N,
           NH3_N, NH4_N, NH3_NH4_N,
           N2O, d15N_NO3,
           TDN, TDKN, TIN, TKN, TN, TPN)

# Ok, it appears that there's an easy delineation for ammonia/um
# values. However, the nitrate is a little more complicated -
# there are not always a matching number of NO2 measurements for
# each of the NO3 measurements (which would result in an uneven
# number of NO3 and NO3_NO2 values in a given record if summed
# which is entirely unhelpful), so I'm going to have it select
# for either NO3 *or* NO3_NO2. In the instances where there are
# both, I am going to select the most numerous rather than
# again combining the two measurement records, so I don't propagate/
# conflate error/variation between NO3 and NO3_NO2 methods.
analyte_counts <- analyte_counts %>%
    mutate(ammo_column = case_when(is.na(NH3_N) & is.na(NH4_N) ~ "NH3_NH4_N",
                                   is.na(NH4_N) & is.na(NH3_NH4_N) ~ "NH3_N",
                                   is.na(NH3_N) & is.na(NH3_NH4_N) ~ "NH4_N",
                                   TRUE ~ NA)) %>%
    mutate(nitr_column = case_when(is.na(NO3_N) | NO3_NO2_N > NO3_N ~ "NO3_NO2_N",
                                   is.na(NO3_NO2_N) | NO3_N > NO3_NO2_N ~ "NO3_N",
                                   TRUE ~ NA))

#### Method investigation ####

# And performing one last investigation into methods.
method_counts <- chem_data %>%
    filter(var %in% c("NH4_N", "NO3_NO2_N", "TDN",
                      "TPN", "NO3_N", "TN",
                      "d15N_NO3", "TIN", "NO2_N",
                      "NH3_N", "TDKN", "TKN",
                      "N2O", "NH3_NH4_N")) %>% # down to ~7million records
    # remove NAs
    drop_na(val) %>% # down to ~2 million records
    # and remove interpolated values
    filter(ms_interp == 0) %>% # down to 400k records
    count(site_code, var, grab_sample) %>%
    ungroup() %>%
    pivot_wider(names_from = c(var, grab_sample), values_from = n) %>%
    select(site_code,
           NO3_N_1, NO3_N_0, NO2_N_1, NO2_N_0, NO3_NO2_N_1, NO3_NO2_N_0,
           NH3_N_1, NH4_N_1, NH3_NH4_N_1,
           N2O_1, d15N_NO3_1, TDN_1, TDN_0,
           TDKN_1, TIN_1, TKN_1, TN_1, TN_0, TPN_1) %>%
    mutate(NO3_flag = case_when(NO3_N_1 > 0 & NO3_N_0 > 0 ~ 1,
                                TRUE ~ 0),
           NO3_NO2_flag = case_when(NO3_NO2_N_1 > 0 & NO3_NO2_N_0 > 0 ~ 1,
                                    TRUE ~ 0),
           # check other analytes as well
           TDN_flag = case_when(TDN_1 > 0 & TDN_0 > 0 ~ 1,
                                TRUE ~ 0),
           TN_flag = case_when(TN_1 > 0 & TN_0 ~ 1,
                               TRUE ~ 0))

# So, all ammonia/um samples are grab samples.
# And nothing to worry about with TDN. But there are
# a few sites that are combining across grab/non-grab samples
# for NO3 and TN. Let's plot these real quick to get a sense of
# their variation.

## Bigelow - NO3 ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "Bigelow") %>%
           filter(var == "NO3_N"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "Bigelow") %>%
           filter(var == "NO3_N"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    theme_bw()

## Bigelow - TN ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "Bigelow") %>%
           filter(var == "TN"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "Bigelow") %>%
           filter(var == "TN"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    theme_bw()

## Marshall Gulch - NO3 ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "MarshallGulch") %>%
           filter(var == "NO3_N"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "MarshallGulch") %>%
           filter(var == "NO3_N"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    scale_x_log10() +
    theme_bw()

## Marshall Gulch - TN ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "MarshallGulch") %>%
           filter(var == "TN"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "MarshallGulch") %>%
           filter(var == "TN"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    scale_x_log10() +
    theme_bw()

## Bear Meadow - NO3_NO2 ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "bear_meadow") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "bear_meadow") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    scale_x_log10() +
    theme_bw()

## Cart Creek - NO3_NO2 ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "cart_creek") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "cart_creek") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    scale_x_log10() +
    theme_bw()

## Ipswich Dam - NO3_NO2 ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "ipswich_dam") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "ipswich_dam") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    scale_x_log10() +
    theme_bw()

## Parked Dam - NO3_NO2 ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "parker_dam") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "parker_dam") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    scale_x_log10() +
    theme_bw()

## Saw Mill Brook - NO3_NO2 ##
ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "saw_mill_brook") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = date,
           y = val,
           color = factor(grab_sample))) +
    geom_point() +
    theme_bw()

ggplot(chem_data %>%
           drop_na(val) %>%
           filter(ms_interp == 0) %>%
           filter(site_code == "saw_mill_brook") %>%
           filter(var == "NO3_NO2_N"),
       aes(x = val,
           fill = factor(grab_sample))) +
    geom_density(alpha = 0.7) +
    scale_x_log10() +
    theme_bw()

# Adding step below so multiples in daily measurements are averaged.

#### Tidy ####

# Step 1 - create baseline nitrogen only dataset
n_data <- chem_data %>%
    # select for variables of interest
    filter(var %in% c("NH4_N", "NO3_NO2_N", "TDN",
                      "TPN", "NO3_N", "TN",
                      "d15N_NO3", "TIN", "NO2_N",
                      "NH3_N", "TDKN", "TKN",
                      "N2O", "NH3_NH4_N")) %>%
    # remove NAs
    drop_na(val) %>%
    # and remove interpolated values
    filter(ms_interp == 0)

# Step 2 - aggregate values by day

n_daily <- n_data %>%
    group_by(site_code, var, date) %>%
    summarize(val_mean = mean(val, na.rm = TRUE)) %>%
    ungroup()

# Step 3 - select analyte identified above
# based on number of observations available
my_selections <- analyte_counts %>%
    select(site_code, ammo_column, nitr_column)

n_select <- n_daily %>%
    pivot_wider(names_from = var, values_from = val_mean) %>%
    full_join(my_selections) %>%
    mutate(ammonia_N = case_when(ammo_column == "NH3_N" ~ NH3_N,
                                 ammo_column == "NH4_N" ~ NH4_N,
                                 ammo_column == "NH3_NH4_N" ~ NH3_NH4_N),
           nitrate_N = case_when(nitr_column == "NO3_N" ~ NO3_N,
                                 nitr_column == "NO3_NO2_N" ~ NO3_NO2_N))

# Step 3 - check for duplicates
n_data_dup <- n_select %>%
    count(date, site_code) # none :)

# Step 4 - trim and export
n_data_trim <- n_select %>%
    select(site_code, date,
           ammonia_N, ammo_column,
           nitrate_N, nitr_column,
           d15N_NO3, N2O, TDN,
           TDKN, TKN, TIN, TN, TPN)

#saveRDS(n_data_trim, "data_working/nitrogen_data_trim_raw.rds")

#### Volume Weighted Means ####

# Tidy discharge data accordingly.
# For normalization by watershed area.
# Normalize by watershed area.
area <- ms_site_data %>%
    select(site_code, ws_area_ha)

# Makes sure all records are distinct.
q_data_tidy <- dplyr::distinct(q_data, site_code, date, .keep_all = TRUE) %>%
    # Removes any interpolated values.
    filter(ms_interp == 0) %>%
    # Adds column in L/s*ha
    left_join(area, by = c("site_code")) %>%
    mutate(val_Lsecha = val/ws_area_ha)
# Leaving in Liters per second per hectare since all nitrogen
# values are expressed in milligrams per Liter.

# And pivoting longer for a moment to make the below simpler.
n_data_trim_long <- n_data_trim %>%
    pivot_longer(cols = c("ammonia_N", "nitrate_N", "d15N_NO3",
                          "N2O", "TDN", "TDKN", "TKN", "TIN",
                          "TN", "TPN"),
                 names_to = "analyte")

# Now to join nitrogen and discharge data.
n_q_data_join <- left_join(n_data_trim_long, q_data_tidy,
                           by = c("site_code", "date"))
# Huh, a quick investigation shows 362 sites at which
# we have N data, but only 217 at which we have discharge.
# So, already losing 40% of sites if we convert to VWM.
# Ah well.

# Calculate monthly VWMs for remaining sites.
n_monthly_vwm <- n_q_data_join %>%
    # Create temporal columns for later grouping.
    mutate(year = year(date),
           month = month(date),
           day = day(date)) %>%
    # Calculate instantaneous C*Q.
    mutate(c_q_instant = value*val_Lsecha) %>%
    # And drop non-existent values.
    drop_na(c_q_instant) %>%
    # Now impose groupings.
    group_by(site_code, analyte, year, month) %>%
    # Calculate mean monthly volume weighted concentrations.
    summarize(monthly_vwm_mgL = (sum(c_q_instant))/(sum(val_Lsecha)),
              n_obs = n()) %>%
    ungroup()

# Export data.
#saveRDS(n_monthly_vwm, "data_working/nitrogen_monthly_VWM.rds")

# Calculate annual VWMs for remaining sites.
n_annual_vwm <- n_q_data_join %>%
    # Create temporal columns for later grouping.
    mutate(year = year(date),
           month = month(date),
           day = day(date)) %>%
    mutate(water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    # Calculate instantaneous C*Q.
    mutate(c_q_instant = value*val_Lsecha) %>%
    # And drop non-existent values.
    drop_na(c_q_instant) %>%
    # Now impose groupings.
    group_by(site_code, analyte, water_year) %>%
    # Calculate mean annual volume weighted concentrations.
    summarize(annual_vwm_mgL = (sum(c_q_instant))/(sum(val_Lsecha)),
              n_obs = n(),
              n_mos = length(unique(month))) %>%
    ungroup()

# Export data.
#saveRDS(n_annual_vwm, "data_working/nitrogen_annual_VWM.rds")

# Quick zipper plot to see how things stack up.
no3_y_df <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    group_by(site_code) %>%
    summarize(n_years = length(unique(water_year))) %>%
    ungroup() # 104 with minimum 10 years data (woot!)

fig <- ggplot(n_annual_vwm %>%
           filter(analyte == "nitrate_N") %>%
           left_join(no3_y_df) %>%
           mutate(site_code = factor(site_code,
                                     levels = unique(site_code[order(n_years)]),
                                     ordered = TRUE)),
       aes(x = water_year, y = site_code)) +
    geom_line() +
    theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

# ggsave(fig,
#        filename = "figures/nitrate_zipper_plot.jpg",
#        height = 8,
#        width = 8,
#        units = "cm",
#        dpi = 300)

# End of script.
