# handle setup
library(here)
source(here('src', 'setup.R'))

# load in q data and remove duplicates
q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F
) %>%
    mutate(water_year = water_year(datetime, origin = 'usgs')) %>%
    distinct(., site_code, datetime, .keep_all = TRUE) %>%
    filter(ms_interp == 0)

# Day in year coverage ####
#count days of year measured
year_counts <- q_data %>%
    group_by(site_code, water_year) %>%
    summarize(n = n())

# make test functions
test_n_effect_wordy <- function(n_test){
paste0('drop ', nrow(filter(year_counts, n > n_test)), ' sites, aka ',
                     100*(1-nrow(filter(year_counts, n > n_test))/nrow(year_counts)), '%')
}

test_n_effect <- function(n_test){
    100*(1-nrow(filter(year_counts, n > n_test))/nrow(year_counts))
}

# apply test functions
tibble(n_test = 12:365) %>%
    rowwise() %>%
    mutate(drop = 100*(1-nrow(filter(year_counts, n > n_test))/nrow(year_counts))) %>%
    ggplot(aes(x = n_test, y = drop))+
    geom_point()+
    labs(x = 'number of days measured in a year',
         y = 'percent of data dropped')

# continuous years coverage ####


#
head(q_data, n = 1000) %>%
ggplot(., aes(x = datetime, y = site_code))+
    geom_point()
