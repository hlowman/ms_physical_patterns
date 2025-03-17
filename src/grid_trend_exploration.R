# handle setup
library(here)
library(RColorBrewer)
source(here('src', 'setup.R'))
library(tmap)
library(usmap)
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
    select(site_code, var, trend, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, flag), names_from = var) %>%
    na.omit() %>%
     mutate(wetting = case_when(flag_ppt == 'increasing' ~ 'W',
                               flag_ppt == 'decreasing' ~ 'D',
                               flag_ppt == 'non-significant' ~ 'N'),
           warming = case_when(flag_tmean == 'increasing' ~ 'H',
                               flag_tmean == 'decreasing' ~ 'C',
                               flag_tmean == 'non-significant' ~ 'N'),
           greening = case_when(flag_GPP == 'increasing' ~ 'G',
                                flag_GPP == 'decreasing' ~ 'B',
                                flag_GPP == 'non-significant' ~ 'N',
                                flag_GPP == NA ~ 'N'),
           grouping = as.factor(paste0(warming, wetting, greening))
           )

grid_groups$grouping <- factor(grid_groups$grouping, levels = c('HDG', #strong down
                                                                            'HDN', 'HNG', 'NDG', # mid down
                                                                            'HDB', 'HNN', 'NNG', 'NDN', # light down
                                                                            'NNN', # no change
                                                                            'HWG', 'HWN', 'HNB', 'NDB', 'NWG', 'HWB', 'CNG', 'CWG', # variable
                                                                            'CNN', 'NNB', 'NWN', #light up
                                                                            'NWB')) # strong up
# write out data ####
write_csv(grid_groups, here('data_working', 'grid_groups.csv'))
grid_groups <- read_csv(here('data_working', 'grid_groups.csv'))

# read in ms data ####
ms_groups <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv')) %>%
    st_as_sf(coords = c("longitude","latitude"), crs = 4326)

# munge ms and grid together ####
ms_temp <- ms_groups %>%
    mutate(source = 'ms') %>%
    select(source, grouping)

grid_temp <- grid_groups  %>%
    mutate(source = 'grid') %>%
    select(source, grouping)

both_groups <- rbind(ms_temp, grid_temp) %>% na.omit()

both_groups %>%
    group_by(source, grouping) %>%
    summarize(n =n()) %>%
    mutate(density = case_when(source == 'grid' ~ n/nrow(grid_temp),
                               source == 'ms' ~ n/nrow(ms_temp))) %>%
ggplot(aes(y = grouping, x = density, fill = source))+
    #geom_density(linewidth=2)+
    geom_col(position = 'dodge')+
    theme_few(base_size = 20)+
    labs(y = 'Grouping',
         x = 'Density',
         fill = 'Dataset')#+
    #scale_fill_manual(labels = c('Grid', 'MacroSheds'), values = c('blue', 'black'))


# gpp scatter

ggplot(grid_groups, aes(x = trend_tmean, y = trend_ppt)) +
    # Points with 'non-significant' flag
    geom_point(data = subset(grid_groups, flag_GPP == "non-significant"),
               color = "grey", size = 2) +
    # Points with other flags
    geom_point(data = subset(grid_groups, flag_GPP != "non-significant"),
               aes(color = trend_GPP), size = 5) +
    scale_color_distiller(palette = 'BrBG', direction = 1) +
    theme_few(base_size = 20) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = 'Temperature trend (mean annual, degrees C)',
         y = 'Precipitation trend (mean annual, mm)',
         color = 'GPP trend (mean annual)')


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

tm_shape(usmap::us_map(exclude = c('AK', 'HI'))) +
    tm_polygons() + # Base US map
    tm_shape(map_data) +
    tm_symbols(
        col = "trend_tmean", # Color by effect size
        title.col = "Temp",
        #shape = "significant", # Different shape for significant points
        #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
        border.col = "black",
        size = 0.1,
        palette = "-RdBu", # Diverging palette for effect size
        legend.col.show = TRUE,
        border.alpha = 0
    ) +
    tm_shape(ms_groups)+
    tm_symbols(col = 'black',
            title.col = 'MS sites',
            legend.col.show = TRUE,
            size = .2)+
    # tm_symbols(
    #     col = "trend_precip_mean", # Color by effect size
    #     title.col = "Sen's slope",
    #     #shape = "significant", # Different shape for significant points
    #     #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
    #     border.col = "black",
    #     size = 0.5,
    #     palette = "-RdBu", # Diverging palette for effect size
    #     legend.col.show = TRUE#,
    #     #legend.shape.show = TRUE
    # )  +
    tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        frame = F,
        legend.title.size = 1,
        legend.text.size = .9,
        legend.outside.size = .4
    )


tm_shape(usmap::us_map(exclude = c('AK', 'HI'))) +
    tm_polygons() + # Base US map
    tm_shape(map_data) +
    tm_symbols(
        col = "trend_ppt", # Color by effect size
        title.col = "PPT",
        #shape = "significant", # Different shape for significant points
        #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
        border.alpha = 0,
        palette = "Spectral", # Diverging palette for effect size
        legend.col.show = TRUE,
        size = .1
        #legend.shape.show = TRUE
    ) +
    tm_shape(ms_groups)+
    tm_symbols(col = 'black',
               title.col = 'MS sites',
               legend.col.show = TRUE,
               size = .2)+
    # tm_symbols(
    #     col = "trend_precip_mean", # Color by effect size
    #     title.col = "Sen's slope",
    #     #shape = "significant", # Different shape for significant points
    #     #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
    #     border.col = "black",
    #     size = 0.5,
    #     palette = "-RdBu", # Diverging palette for effect size
    #     legend.col.show = TRUE#,
    #     #legend.shape.show = TRUE
    # ) +
    tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        frame = F,
        legend.title.size = 1,
        legend.text.size = .9,
        legend.outside.size = .4
    )

tm_shape(usmap::us_map(exclude = c('AK', 'HI'))) +
    tm_polygons() + # Base US map
    tm_shape(map_data) +
    tm_symbols(
        col = "trend_GPP", # Color by effect size
        title.col = "GPP",
        #shape = "significant", # Different shape for significant points
        #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
        border.alpha = 0,
        palette = "BrBG", # Diverging palette for effect size
        legend.col.show = TRUE,
        size = .1
        #legend.shape.show = TRUE
    ) +
    tm_shape(ms_groups)+
    tm_symbols(col = 'black',
               title.col = 'MS sites',
               legend.col.show = TRUE,
               size = .2)+
    tm_add_legend(
        type = "symbol",
        labels = "MS sites", # Legend label
        col = "black", # Color matching the points
        shape = 16, # Default filled circle shape
        size = .5
    ) +
    # tm_symbols(
    #     col = "trend_precip_mean", # Color by effect size
    #     title.col = "Sen's slope",
    #     #shape = "significant", # Different shape for significant points
    #     #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
    #     border.col = "black",
    #     size = 0.5,
    #     palette = "-RdBu", # Diverging palette for effect size
    #     legend.col.show = TRUE#,
    #     #legend.shape.show = TRUE
    # ) +
    tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        frame = F,
        legend.title.size = 1,
        legend.text.size = .9,
        legend.outside.size = .4
    )

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
