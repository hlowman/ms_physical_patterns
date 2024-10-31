library(tidyverse)
library(here)
library(sf)
library(arcgislayers)
library(nhdplusTools)
library(rnaturalearth)
source(here('src', 'setup.R'))

# ms gauges ####
# geospatial aoi = a polygon or point sf object. either work
# geospatial_aoi <- full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
#     add_flags() %>%
#     left_join(.,ms_site_data, by = 'site_code') %>%
#     select(site_code, latitude, longitude) %>%
#     distinct() %>%
#     st_as_sf(coords = c("longitude","latitude"), crs = 4326)

geospatial_aoi <- ms_load_spatial_product(macrosheds_root = my_ms_dir,
                                          spatial_product = 'stream_gauge_locations',
                                          networks = 'neon')

us_states <- ne_states(country = "United States of America", returnclass = "sf")


mapview(geospatial_aoi)

output <- tibble(site_code = as.character(),
                 stream_order = as.integer())

for(i in 1:nrow(geospatial_aoi)){

try(
nearest_comid <- discover_nhdplus_id(geospatial_aoi[i,]), silent = T
)
if(exists('nearest_comid') & !is.null(nearest_comid)){

flowline_data <- get_nhdplus(comid = nearest_comid)

inner = tibble(site_code = geospatial_aoi$site_code[i],
               stream_order = flowline_data$streamorde)

rm(nearest_comid)

}else{inner = tibble(site_code = geospatial_aoi$site_code[i],
                     stream_order = NA)}

output <- rbind(output, inner)
}
# Extract the stream order from the flowline data



