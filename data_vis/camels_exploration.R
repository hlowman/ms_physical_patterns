library(here)
library(devtools)
library(nhdplusTools)
source(here('src', 'setup.R'))
#install_github('scantle/caRmels')

library(caRmels)
camels_dir <- here('data_raw', 'camels')

# record length ####
gauge_info_path <- file.path(camels_dir,'basin_dataset_public_v1p2/basin_metadata/gauge_information.txt')
gauges <- read.gaugeInfo(gauge_info_path)

gg <- gauges %>%
    filter(drainage_area_km2 <= 100)

basinSubset <- gg$gage_ID

flow_dir <- file.path(camels_dir,'basin_dataset_public_v1p2/usgs_streamflow')
streamflows <- importStreamflows(flow_dir, subset = basinSubset)

str(streamflows)


q_df <- tibble(site_code = as.character(),
               date = as.character(),
               streamflow_cfs = as.numeric(),
               QC_flag = as.character())

for(i in 1:length(streamflows)){

site_code <- names(streamflows)[i]
inner_df <- streamflows[[i]] %>%
    tibble() %>%
    mutate(site_code = site_code)

q_df <- rbind(q_df, inner_df)
    }

years <- q_df %>%
    mutate(date = as.Date(date),
           water_year = as.integer(as.character(water_year(date, origin = 'usgs')))
                                   ) %>%
    group_by(site_code, water_year) %>%
    summarize(n = n()) %>%
    group_by(site_code) %>%
    summarize(n = n()) %>%
    filter(n >= 10)

min(years$n)
max(years$n)


# stream order pull ####
str_ord <- head(years, n = 0) %>%
    mutate(order = as.numeric())
for(i in unique(years$site_code)){
n <- filter(years, site_code == i) %>%
    .$n
#if(i == "01054200"){next}
nldi_nwis <- list(featureSource = "nwissite", featureID = paste0("USGS-", i))
target_comid <- discover_nhdplus_id(nldi_feature = nldi_nwis)

flowlines <- navigate_nldi(
    list(featureSource = "comid", featureID = target_comid),
    mode = "upstreamTributaries",  # Find upstream flowlines
    data_source = "flowlines"
)
mapview(flowlines)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = T,
                         return_data = TRUE, overwrite = TRUE, streamorder = T)

target_str_ord <- subset$NHDFlowline_Network %>%
    select(comid, streamorde) %>%
    filter(comid == target_comid) %>%
    .$streamorde

inner <- tibble(site_code = i,
                n = n,
                order = target_str_ord)

str_ord <- rbind(str_ord, inner)

}

good_ord <- str_ord %>%
    filter(order < 2)

write_csv(good_ord, file = 'good_nwis_sites.csv')

# final filtering ####
good_ord %>%
    left_join(., gauges, by = c('site_code' = 'gage_ID')) %>%
    mutate(ws_size_group = case_when(drainage_area_km2 < 10 ~ 'under 10',
                                drainage_area_km2 > 20 ~ 'above 20',
                                .default = 'between 10 and 20')) %>%
    st_as_sf(coords = c("long","lat"), crs = 4326) %>%
    mapview(., zcol = 'ws_size_group')

gg %>%
    ggplot(aes(x = drainage_area_km2))+
    geom_histogram()+
    theme_few(base_size = 20)+
    labs(x = 'WS Area (km2)',
         title = 'CAMELS sites',
         caption = paste0('n=',nrow(years)))

# size comparison of ms sites ####
ms_sites <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv')) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    filter(grouping != 'data limited') %>%
    transmute(site_code, lat = latitude, long = longitude, drainage_area_km2 = (ws_area_ha*0.01), source = 'ms') %>%
    filter(!site_code %in% c('ARIK', 'TOMB', 'BLWA', 'FLNT'))

camels_sites <- gg %>%
    select(site_code = gage_ID, lat, long, drainage_area_km2) %>%
    mutate(source = 'camels_sub_50km2')

comb_sites <- rbind(ms_sites, camels_sites)

comb_sites %>%
    ggplot(aes(x = drainage_area_km2))+
    geom_histogram()+
    theme_few(base_size = 20)+
    #scale_x_log10()+
    labs(x = 'WS Area (km2)',
         #caption = paste0('n=',nrow(years))
         )+
    facet_wrap(~source)+
    labs(caption = 'omitting ARIK, TOMB, BLWA, FLNT from ms side as they are all 1000km or greater')

