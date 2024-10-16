library(tidyverse)
library(sf)
library(arcgislayers)

# geospatial aoi = a polygon or point sf object. either work
geospatial_aoi <- st_read("CLP_NW_ROSS_points.gpkg")[1:100,] %>%
    st_as_sfc()

nhd_plus_hr_url <- "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer"

# open the nhd_hr - which contains a bunch of layers
nhd_hr <- arcgislayers::arc_open(nhd_plus_hr_url)

# list the layers of the nhdhr object
arcgislayers::list_items(nhd_hr)

# select the layer by id from the items list called above (10 is HR catchments)
nhd_hr_catchments <- arcgislayers::get_layer(nhd_hr, 10)

# use aoi to return associated catchments
nhd_catchments <- vector("list", length = length(geospatial_aoi))

for(i in 1:length(geospatial_aoi)){
    try(nhd_catchments[[i]] <- arcgislayers::arc_select(nhd_hr_catchments,
                                                        filter_geom = geospatial_aoi[i],
                                                        crs = sf::st_crs(geospatial_aoi[i])) %>%
            sf::st_make_valid(), silent = TRUE)
}

nhd_catchments <- nhd_catchments %>%
    purrr::keep(~!is.null(.)) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

# use catchments to grab intersecting NHDHR flowline features
geospatial_aoi <- nhd_catchments %>%
    sf::st_make_valid() %>%
    sf::st_as_sfc()

# select the layer by id from the items list called above (3 is HR flowlines)
nhd_hr_flowlines <- arcgislayers::get_layer(nhd_hr, 3)

# Return flowlines associated with intersecting NHD catchment features
nhd_flowlines <- vector("list", length = length(geospatial_aoi))

for(i in 1:length(geospatial_aoi)){
    try(nhd_flowlines[[i]] <- arcgislayers::arc_select(nhd_hr_flowlines,
                                                       filter_geom = geospatial_aoi[i],
                                                       crs = sf::st_crs(geospatial_aoi[i])) %>%
            sf::st_make_valid(), silent = TRUE)

    try(geometry_col <- sf::st_geometry(nhd_flowlines[[i]])
        , silent = TRUE)

    try(nhd_flowlines[[i]] <- nhd_flowlines[[i]] %>%
            dplyr::mutate(dplyr::across(dplyr::where(~ !identical(., geometry_col)), ~ as.character(.)))
        , silent = TRUE)
}


# final object is a list of all NHDHR flowlines that intersect the same NHD HR catchments as your areas of interest.
# stream order is found in the "streamorde" column
nhd_flowlines <- nhd_flowlines %>%
    purrr::keep(~!is.null(.)) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()
