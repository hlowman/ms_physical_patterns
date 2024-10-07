# clear environment
#rm(list = ls())
# Load packages.
library(here)
source(here('src', 'setup.R'))
#source(here('src', 'mega_zipper_data.R'))
flag_colors <- c('increasing' = "red", 'decreasing' = 'blue', 'non-significant' = "grey", 'data limited' = 'purple4')

q_trends <- read_csv(here('data_working', 'trends', 'best_run_prisim.csv')) %>%
    filter(var == 'q_mean') %>%
    select(site_code, q_trend = trend, q_p = p)


# read in data####
full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
    add_flags() %>%
    select(site_code, var, trend, p, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, p), names_from = var) %>%
    na.omit() %>%
    mutate(wetting = case_when(trend_precip_mean_ann > 0 & p_precip_mean_ann <= 0.05 ~ 'W',
                               trend_precip_mean_ann < 0 & p_precip_mean_ann <= 0.05 ~ 'D',
                               p_precip_mean_ann > 0.05 ~ 'N'),
           warming = case_when(trend_temp_mean_ann > 0 & p_temp_mean_ann <= 0.05 ~ 'H',
                                trend_temp_mean_ann < 0 & p_temp_mean_ann <= 0.05 ~ 'C',
                                p_temp_mean_ann > 0.05 ~ 'N'),
           greening = case_when(trend_gpp_conus > 0 & p_gpp_conus <= 0.05 ~ 'G',
                                trend_gpp_conus < 0 & p_gpp_conus <= 0.05 ~ 'B',
                                p_gpp_conus > 0.05 ~ 'N',
                                trend_gpp_conus == 0 ~ 'N'),
           grouping = as.factor(paste0(warming, wetting, greening))
           ) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    mutate(grouping_exp = case_when(ws_status == 'experimental' ~ 'EXP',
                                .default = 'NON')) %>%
    left_join(., q_trends, by = 'site_code') %>%
    mutate(streamflow = case_when(q_trend > 0 & q_p <= 0.05 ~ 'increasing',
                                  q_trend < 0 & q_p <= 0.05 ~ 'decreasing',
                                  q_p > 0.05 ~ 'non-significant',
                                  .default = 'data limited'),
           streamflow = as.factor(streamflow))

full_prism_trends$grouping <- factor(full_prism_trends$grouping, levels = c('HDG', #strong down
                                                                            'HDN', 'HNG', 'NDG', # mid down
                                                                            'HDB', 'HNN', 'NNG', 'NDN', # light down
                                                                            'NNN', # no change
                                                                            'HWG', 'HWN', 'HNB', 'NDB', 'NWG', # variable
                                                                            'NNB', 'NWN', #light up
                                                                            'NWB')) # strong up



ggplot(full_prism_trends, aes(x = grouping, fill = streamflow))+
    geom_bar()+
    theme_few(base_size = 20)+
    scale_fill_manual(values = flag_colors)#,

ggplot(full_prism_trends, aes(x = grouping, fill = streamflow))+
    geom_bar()+
    theme_few(base_size = 20)+
    theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = .5))+
    scale_fill_manual(values = flag_colors)+#,
    facet_wrap(~grouping_exp)


full_prism_trends %>%
    filter(grouping_exp == 'EXP',
           streamflow == 'decreasing') %>%
    select(domain, site_code)

full_prism_trends %>%
    filter(grouping_exp == 'NON',
           streamflow == 'increasing') %>%
    select(domain)

full_prism_trends %>%
    filter(grouping_exp == 'NON',
           streamflow == 'decreasing') %>%
    select(domain)

View(full_prism_trends %>%
    filter(grouping == 'NNG') %>%
    select(domain, site_code))


metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS')) %>%
    distinct()

metrics %>%
    filter(site_code == 'GSWS06', water_year > 1980) %>%
    ggplot(aes(x = water_year, y = gpp_conus))+
    geom_point()
