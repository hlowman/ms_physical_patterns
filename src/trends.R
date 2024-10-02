# handle setup
library(here)
source(here('src', 'setup.R'))

# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS'))

# run make table of all cut dates of interest
cut_dates <- tibble(prod = c('prism', 'landsat_5', 'modis'),
                    water_year = c(1980, 1984, 2000))

# loop through table to make trends of each
foreach(i = 1:nrow(cut_dates)) %do% {

    prod <- cut_dates$prod[i]
    start_year <- cut_dates$water_year[i]
    long_name <- paste0('longest_site_run_', prod)

    inner <- metrics %>%
        select(-contains('date')) %>%
        pivot_longer(cols = -c('site_code', 'water_year'), names_to = 'var', values_to = 'val')   %>%
        filter(water_year >= start_year) %>%
        reduce_to_longest_site_runs(., 'q_mean') %>%
        detect_trends(., diag_string = long_name) %>%
        add_flags()

    write_csv(inner, here('data_working', 'trends', paste0(long_name, '.csv')))

}