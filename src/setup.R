# setup script to save effort when using v2 without package

# Note, this script uses a newer version of the data (v2) sent by
# Mike on 8/19/24, not the current package version of the dataset.

# Load common packages.
## core
library(here)
library(macrosheds)
## data vis
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(viridis)
library(patchwork)
library(ggforce)
library(mapview)
library(plotly)
library(ggplotify)
library(gt)
## stats
library(e1071)
library(lfstat)
library(trend)
## data carpentry
library(naniar)
library(lubridate)
library(tidyverse)
library(sf)
## util
library(feather)

# Using revised code from Mike to point to new data.
rdata_path <- "data_raw" # updated path

load(file.path(rdata_path, 'ms_site_data.RData'))
load(file.path(rdata_path, 'ms_vars_ws_attr.RData'))
load(file.path(rdata_path, 'ms_vars_ts.RData'))
load(file.path(rdata_path, 'ms_var_catalog.RData'))

nv <- as.environment('package:macrosheds')

for(ms_data in c('ms_vars_ts', 'ms_vars_ws', 'ms_site_data', 'ms_var_catalog')){
    unlockBinding(ms_data, nv)
    assign(ms_data, get(ms_data), envir = nv)
    lockBinding(ms_data, nv)
}

# Set directory to folder containing new data.
my_ms_dir <- "data_raw"

# set master vars
source(here('src', 'set_master_coverage_vars.R'))

# helper functions ####

# freq_check needs a data frame of long format q_data with water_year
frequency_check <- function(data_in){

    q_data <- data_in

    freq_check <- q_data %>%
    mutate(week_year = paste0(week(datetime), '_', water_year)) %>%
    group_by(site_code, week_year) %>%
    summarize(water_year = max(water_year),
              n = n()) %>%
    filter(n >= minimum_per_week_sampling_frequency_master) %>%
    group_by(site_code, water_year) %>%
    summarize(n = n())

    return(freq_check)
}
