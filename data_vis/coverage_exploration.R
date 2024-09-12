# handle setup
library(here)
source(here('src', 'setup.R'))

# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS'))

# trend under diff sat coverage ####
# test where we have at least 10 years of data post landsat
q_landsat_good <- metrics %>%
    select(site_code, water_year, m1_meanq) %>%
    filter(water_year >= landsat_year) %>%
    reduce_to_longest_site_runs() %>%
    group_by(site_code) %>%
    summarize(n_q = n()) %>%
    filter(n_q >= 10)

t_landsat_good <- metrics %>%
    select(site_code, water_year, temp_mean_ann) %>%
    filter(water_year >= landsat_year) %>%
    reduce_to_longest_site_runs() %>%
    group_by(site_code) %>%
    summarize(n_temp = n()) %>%
    filter(n_temp >= 10)

com_good <- q_landsat_good %>%
    full_join(., t_landsat_good, by = 'site_code') %>%
    na.omit()%>%
    left_join(., ms_site_data, by = 'site_code')

com_good %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    mapview()

nrow(com_good)

# t test evaluation ####
# max gap
bin_old <- 1950:1974
bin_new <- 2000:2024

old_good <- metrics %>%
    select(site_code, water_year, m1_meanq) %>%
    filter(water_year %in% bin_old) %>%
    group_by(site_code) %>%
    summarize(n_old = n())%>%
    filter(n_old > 9)

new_good <- metrics %>%
    select(site_code, water_year, m1_meanq) %>%
    filter(water_year %in% bin_new) %>%
    group_by(site_code) %>%
    summarize(n_new = n()) %>%
    filter(n_new > 9)

both_good <- full_join(old_good, new_good) %>%
    na.omit()

both_good %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    mapview()

# max data
bin_old <- 1980:2000
bin_new <- 2001:2024

old_good <- metrics %>%
    select(site_code, water_year, m1_meanq) %>%
    filter(water_year %in% bin_old) %>%
    group_by(site_code) %>%
    summarize(n_old = n())%>%
    filter(n_old > 9)

new_good <- metrics %>%
    select(site_code, water_year, m1_meanq) %>%
    filter(water_year %in% bin_new) %>%
    group_by(site_code) %>%
    summarize(n_new = n()) %>%
    filter(n_new > 9)

both_good <- full_join(old_good, new_good) %>%
    na.omit()

nrow(both_good)
unique((both_good %>%left_join(., ms_site_data, by = 'site_code'))$domain)


both_good %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    mapview()

# compromise
bin_old <- 1980:1994
bin_new <- 2010:2024

old_good <- metrics %>%
    select(site_code, water_year, m1_meanq) %>%
    filter(water_year %in% bin_old) %>%
    group_by(site_code) %>%
    summarize(n_old = n())%>%
    filter(n_old > 9)

new_good <- metrics %>%
    select(site_code, water_year, m1_meanq) %>%
    filter(water_year %in% bin_new) %>%
    group_by(site_code) %>%
    summarize(n_new = n()) %>%
    filter(n_new > 9)

both_good <- full_join(old_good, new_good) %>%
    na.omit()

nrow(both_good)
unique((both_good %>%left_join(., ms_site_data, by = 'site_code'))$domain)


both_good %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    mapview()
