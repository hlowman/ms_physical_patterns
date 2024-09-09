# handle setup
library(here)
source(here('src', 'setup.R'))

# set cutoffs ####
start_year_master <- 2001
minimum_per_week_sampling_frequency_master <- 4
good_weeks_to_year_master <- 51
minimum_continuous_record_length_master <- 10

domains <- ms_site_data %>%
    select(domain, site_code)

# 0 Read in q data ####
# load in q data - 1970688 observations at 217 sites, 29 domains
q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F) %>%
    mutate(water_year = as.integer(as.character(water_year(datetime, origin = 'usgs')))) %>%
    distinct(., site_code, datetime, .keep_all = TRUE) %>%
    left_join(.,domains, by = c('site_code'))

# and remove duplicates - 1480264 observations at 217 sites, 29 domains
q_data <- q_data %>%
    filter(ms_interp == 0)

q_data <- q_data %>%
# 1 Filter to start year #### - 823008 observations at 213 sites, 29 domains
    filter(water_year >= start_year_master) # filter for MODIS sat data

# 2 Data frequency check ####
# only want sites that are reporting q at 4 times a week
# note this spits out number of weeks in a given water year
freq_check <- q_data %>%
    mutate(week_year = paste0(week(datetime), '_', water_year)) %>%
    group_by(site_code, week_year) %>%
    summarize(water_year = max(water_year),
              n = n()) %>%
    filter(n >= minimum_per_week_sampling_frequency_master) %>%
    group_by(site_code, water_year) %>%
    summarize(n = n())

# get an idea of what a cutoff could/should be
freq_check %>%
    ggplot(., aes(x = n)) +
    geom_histogram()

## make tibble of good site years for data frequency ####
good_site_year_freq <- freq_check %>%
    filter(n >= good_weeks_to_year_master) %>%
    select(site_code, water_year)

test_freq <- freq_check %>%
    filter(n > 50) %>%
    nrow()

test_freq - (good_site_year_freq %>%
    nrow())

# additional check for sensitivity analysis - 662374 observations at 159 sites,
# 26 domains
q_double_check <- q_data %>%
    right_join(., good_site_year_freq, by = c('site_code', 'water_year'))

# making an extra plot to visualize frequency of data
ggplot(freq_check %>%
           left_join(domains), aes(x = domain, y = n,
                                   color = domain)) +
    geom_jitter(alpha = 0.8) +
    labs(y = "# Weeks with min. 4 obs.") +
    theme_bw() +
    theme(legend.position = "none")

# DEFUNCT | Day in year coverage ####
# NOTE: commenting out this section for now as our frequency coverage makes this redundant
#count days of year measured
# year_counts <- q_data %>%
#     group_by(site_code, water_year) %>%
#     summarize(n = n())
#
# # make test functions
# test_n_effect_wordy <- function(n_test){
# paste0('drop ', nrow(filter(year_counts, n > n_test)), ' sites, aka ',
#                      100*(1-nrow(filter(year_counts, n > n_test))/nrow(year_counts)), '%')
# }
#
# test_n_effect <- function(n_test){
#     100*(1-nrow(filter(year_counts, n > n_test))/nrow(year_counts))
# }
#
# # apply test functions
# tibble(n_test = 12:365) %>%
#     rowwise() %>%
#     mutate(drop = 100*(1-nrow(filter(year_counts, n > n_test))/nrow(year_counts))) %>%
#     ggplot(aes(x = n_test, y = drop))+
#     geom_point()+
#     labs(x = 'number of days measured in a year',
#          y = 'percent of data dropped')
#
# # filter to a given number
#
# full_years <- year_counts %>%
#     filter(n > 300) %>%
#     mutate(water_year = as.integer(as.character(water_year)))
#

# 3 continuous year coverage ####
# # look at years
year_run_check <- q_data %>%
    right_join(., good_site_year_freq, by = c('site_code', 'water_year'))%>%
    arrange(site_code, water_year) %>%
    group_by(site_code, water_year) %>%
    summarize(n = n())

## find longest run in each site ####
# initialize output
out_frame <- tibble(site_code = as.character(), water_year = as.integer(), n = as.integer())

# loop through sites
for(i in unique(year_run_check$site_code)){
target_site <- i

site_data <- year_run_check %>%
    filter(site_code == target_site)

years <- site_data$water_year
# https://stackoverflow.com/questions/26639110/find-longest-consecutive-number-in-r
s <- split(years, cumsum(c(TRUE, diff(years) != 1)))
out_years <- s[[which.max(lengths(s))]]

out_data <- site_data %>%
    filter(water_year %in% out_years)

out_frame <- rbind(out_frame, out_data)
}

# now check record lengths
longest_runs <- out_frame %>%
    distinct(site_code, water_year) %>%
    group_by(site_code) %>%
    summarize(record_length = n(),
              start = min(water_year),
              end = max(water_year)) %>%
    arrange(-record_length)

# get shape of data
ggplot(longest_runs, aes(x = record_length))+
    geom_histogram()+
    theme_few()+
    geom_vline(xintercept = 10, color = 'red')

print(longest_runs %>%
          arrange(-record_length), n = 20)

# what does a 10 year cut look like?
test <- longest_runs %>%
    filter(record_length > 9) %>%
    left_join(., ms_site_data, by = 'site_code')

unique(test$domain)

ggplot(test, aes(y = site_code, color = domain))+
    geom_segment(aes(x = start, xend = end, yend = site_code))+
    theme_few()+
    scale_color_viridis(discrete = T)

#how about 20?
test <- longest_runs %>%
    filter(record_length > 19) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    arrange(domain)

unique(test$domain)

ggplot(test, aes(y = site_code, color = domain))+
    geom_segment(aes(x = start, xend = end, yend = site_code))+
    theme_few()+
    scale_color_viridis(discrete = T)

## make tibble of good site years ####
good_site_years <- longest_runs %>%
    filter(record_length >= minimum_continuous_record_length_master)

# and some last few sensitivity checks
# for both 10 years, - 509072 observations at 74 sites, 18 domains
q_triple_check10 <- longest_runs %>%
    filter(record_length > 9) %>%
    left_join(., q_double_check, by = c('site_code'))

# and for 20 years. - 282313 observations at 35 sites, 8 domains
q_triple_check20 <- longest_runs %>%
    filter(record_length > 19) %>%
    left_join(., q_double_check, by = c('site_code'))

# save output for use in q_metrics
saveRDS(good_site_years, here('data_working', 'good_site_years.RDS'))
