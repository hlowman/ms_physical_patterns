library(macrosheds)

ms_root <- '~/ssd2/macrosheds_stuff/ms_test/'

# daily precip (mm) ####

# remember, "median" in this variable name refers to the way data were aggregated spatially.
# These are daily totals in mm.

# ms_download_ws_attr(ms_root, "time series", timeout = 10000, skip_existing = TRUE)
p <- ms_load_product(ms_root,
                     prodname = 'ws_attr_timeseries:climate',
                     filter_vars = 'precip_median',
                     warn = FALSE) %>%
    select(-var, -year, -pctCellErr, precip_median = val)

# daily pET (mm) ####

#this dataset also includes precip, from daymet instead of prism

# ms_download_ws_attr(ms_root, "CAMELS Daymet forcings", timeout = 10000, skip_existing = TRUE)
daymet <- ms_load_product(ms_root,
                          prodname = 'ws_attr_CAMELS_Daymet_forcings',
                          warn = FALSE)

# aridity index ####

# this is the same way it's calculated by CAMELS, so should be same as "aridity" in
# ws_attr_CAMELS_summaries, but daily. (https://gdex.ucar.edu/dataset/camels/file/camels_attributes_v2.0.xlsx)

d <- inner_join(p, daymet, by = c('network', 'domain', 'site_code', 'date')) %>%
    mutate(aridity_index = `pet(mm)` / precip_median) # *

# * caveat! you'll find that p/pet is another common aridity index. GPT has all sorts
# of explanations for why CAMELS uses pet/p instead, including that it fits better
# with the Budyko framework. Still, you might want to check mean(pet) / mean(p) to see if it matches
# the aridity values given in ws_attr_CAMELS_summaries. It'll be most similar if you use "prcp(mm/day)"
# from daymet instead of "precip_median" from prism.

