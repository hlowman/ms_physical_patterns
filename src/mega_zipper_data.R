# handle setup
library(here)
source(here('src', 'setup.R'))

# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS')) %>%
    distinct()

# run full climate trends first
clim_trends <- metrics %>%
    select(-contains('date')) %>% # dates breaking math
    pivot_longer(cols = -c('site_code', 'water_year'),
                 names_to = 'var',
                 values_to = 'val') %>%
    filter(var %in% c('temp_mean_ann', 'precip_mean_ann', 'gpp_conus')) %>%
    distinct() %>%
    reduce_to_longest_site_runs(., metric = 'temp_mean_ann') %>%
    detect_trends(.,'full_prisim')

write_csv(clim_trends, here('data_working', 'trends', 'full_prisim_climate.csv'))

# check nas from climate run
clim_trends %>%
    filter(is.na(trend)) %>%
    select(site_code) %>%
    distinct()

# create longest run w/ prism data ####
# make frame of all data during prism
prism_site_run_trends_data <- metrics %>%
    select(-contains('date')) %>% # dates breaking math
    select(site_code, water_year,
           # climate
           temp_mean_ann,
           precip_mean_ann,
           p_q50_dowy_exceed = p50_dowy_exceed,
           p_n_days = ppt_days,
           p_mean_intensity = precip_total_ann_days,
           # hydro
           q_mean, q_ar1, q_rbi,
           q_q50_dowy_exceed = q50_exceed,
           runoff_ratio,
           q_q25_oct = q25_oct,
           # in-stream
           stream_temp_mean_ann,
           stream_temp_min_winter,
           stream_temp_mean_summer,
           # productivity
           gpp_conus = gpp_CONUS_30m_median) %>%
    #drop_na(temp_mean_ann, precip_mean_ann, q_mean) %>%
    pivot_longer(cols = -c('site_code', 'water_year'),
                 names_to = 'var',
                 values_to = 'val') %>%
           filter(water_year >= prisim_year) %>%
    reduce_to_best_range(., metric = 'q_mean')

# trend detection ####
prism_site_run_trends <- detect_trends(prism_site_run_trends_data, 'site_run_prism')

# export
#write_csv(prism_site_run_trends , here('data_working', 'trends', 'longest_site_run_prisim.csv'))
write_csv(prism_site_run_trends , here('data_working', 'trends', 'best_run_prisim.csv'))
