# setup script to save effort when using v2 without package

# Note, this script uses a newer version of the data (v2) sent by
# Mike on 8/19/24, not the current package version of the dataset.

# Load common packages.
library(here)
library(tidyverse)
library(ggplot2)
library(naniar)
library(lubridate)
library(feather)
library(macrosheds)
library(feather)
library(lfstat)
library(trend)
library(ggrepel)
library(ggthemes)
library(viridis)
library(patchwork)
library(ggforce)
library(e1071)

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