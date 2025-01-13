# handle setup
library(here)
library(RColorBrewer)
source(here('src', 'setup.R'))

# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))

grid_metrics <- read_csv(here('data_raw', 'grid_csvs', 'et2.csv')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'gpp.csv')), by = c('FID', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'pdsi.csv')), by = c('FID', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'ppt.csv')), by = c('FID', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'spei30d.csv')), by = c('FID', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'temperature.csv')), by = c('FID', 'year')) %>%
    select(site_code = FID, water_year = year, geometry = geometry.x, ET, GPP, pdsi, ppt, spei30d, tmean) %>%
    pivot_longer(cols = -c('water_year', 'site_code', 'geometry'), names_to = 'var', values_to = 'val')

grid_trends <- grid_metrics %>%
    detect_trends()

#write_csv(grid_trends, here('data_working', 'grid_trends.csv'))
grid_trends <- read_csv(here('data_working', 'grid_trends.csv')) %>%
    add_flags()

# data vis ####
test <- grid_trends %>%
    add_flags()

ggplot(test, aes(x = flag)) +
    geom_bar() +
    facet_wrap(~var, ncol = 1) +
    theme_few()

# data grouping ####
grid_groups <- grid_trends %>%
    select(site_code, var, trend, p, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, p), names_from = var) %>%
    na.omit() %>%
    mutate(wetting = case_when(trend_ppt > 0 & p_ppt <= 0.05 ~ 'W',
                               trend_ppt < 0 & p_ppt <= 0.05 ~ 'D',
                               p_ppt > 0.05 ~ 'N'),
           warming = case_when(trend_tmean > 0 & p_tmean <= 0.05 ~ 'H',
                               trend_tmean < 0 & p_tmean <= 0.05 ~ 'C',
                               p_tmean > 0.05 ~ 'N'),
           greening = case_when(trend_GPP > 0 & p_GPP <= 0.05 ~ 'G',
                                trend_GPP < 0 & p_GPP <= 0.05 ~ 'B',
                                p_GPP > 0.05 ~ 'N',
                                trend_GPP == 0 ~ 'N'),
           grouping = as.factor(paste0(warming, wetting, greening))
    )

grid_groups$grouping <- factor(grid_groups$grouping, levels = c('HDG', #strong down
                                                                            'HDN', 'HNG', 'NDG', # mid down
                                                                            'HDB', 'HNN', 'NNG', 'NDN', # light down
                                                                            'NNN', # no change
                                                                            'HWG', 'HWN', 'HNB', 'NDB', 'NWG', 'HWB', 'CNG', 'CWG', # variable
                                                                            'CNN', 'NNB', 'NWN', #light up
                                                                            'NWB')) # strong up

write_csv(grid_groups, here('data_working', 'grid_groups.csv'))
grid_groups <- read_csv(here('data_working', 'grid_groups.csv'))

ggplot(grid_groups, aes(x = grouping))+
    geom_bar()+
    theme_few(base_size = 20)+
    scale_fill_manual(values = flag_colors)


# maps ####
lat_lon_pattern <- "c\\((-?\\d+\\.\\d+),\\s*(-?\\d+\\.\\d+)\\)"


map_data <- grid_metrics %>%
    select(site_code, geometry) %>%
    distinct() %>%
    mutate(
        Longitude = as.numeric(gsub("c\\((.*),.*", "\\1", geometry)),
        Latitude = as.numeric(gsub("c\\(.*,(.*)\\)", "\\1", geometry))
    ) %>%
    left_join(grid_groups, by = 'site_code', relationship = 'many-to-many') %>%
    na.omit() %>%
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

# map_data %>%
#     filter(var == 'tmean') %>%
#     mapview(., zcol = 'flag')
#
# map_data %>%
#     filter(var == 'ET') %>%
#     mapview(., zcol = 'flag')
#
# map_data %>%
#     filter(var == 'ppt') %>%
#     mapview(., zcol = 'flag')
#
# map_data %>%
#     filter(var == 'pdi') %>%
#     mapview(., zcol = 'flag')
#
# map_data %>%
#     filter(var == 'GPP') %>%
#     mapview(., zcol = 'flag')

pal = mapviewPalette("mapviewTopoColors")
map_data %>%
    mapview(., zcol = 'trend_ET', col.regions=rev(brewer.pal(10, "Spectral")))

map_data %>%
    mapview(., zcol = 'trend_GPP', col.regions=brewer.pal(10, "PiYG"))

map_data %>%
    filter(p_GPP < 0.1) %>%
    mapview(., zcol = 'trend_GPP', col.regions=brewer.pal(10, "PiYG"))


map_data %>%
    mapview(., zcol = 'trend_ppt', col.regions=brewer.pal(10, "Spectral"))

map_data %>%
    filter(p_tmean < 0.05) %>%
    mapview(., zcol = 'trend_tmean', col.regions=rev(brewer.pal(10, "Spectral")))
