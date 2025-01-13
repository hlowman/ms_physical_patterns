read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
    add_flags() %>%
    select(site_code, var, trend, p, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, p), names_from = var) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    na.omit() %>%
    mutate(warming = case_when(trend_median_air_temp_winter > 0 & p_median_air_temp_winter <= 0.05 ~ 'H',
                               trend_median_air_temp_winter < 0 & p_median_air_temp_winter <= 0.05 ~ 'C',
                               p_median_air_temp_winter > 0.05 ~ 'N')) %>%
    filter(warming == 'H' | warming == 'C') %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    mapview(., zcol = 'trend_median_air_temp_winter')

