# setup script to save effort when using v2 without package

# Note, this script uses a newer version of the data (v2) sent by
# Mike on 8/19/24, not the current package version of the dataset.

# Load common packages.
## core
library(here)
library(macrosheds)
library(foreach)
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

# set master vars for data coverage to define 'good years'
# shared across all data coverage scripts
start_year_master <- 2001
minimum_per_week_sampling_frequency_master <- 4
good_weeks_to_year_master <- 51
minimum_continuous_record_length_master <- 10


# set flag colors for plots
flag_colors <- c('increasing' = "red", 'decreasing' = 'blue', 'flat' = 'green', 'non-significant' = "grey")

# set sat launch years
prisim_year <- 1980
landsat_year <- 1984
modis_year <- 2000


# helper functions ####

## CARPENTRY ####

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
    summarize(n = n()) %>%
    filter(n >= good_weeks_to_year_master)

    return(freq_check)
}

# reduces long data frame to years of longest run within that dataframe per site
reduce_to_longest_site_runs <- function(data_in, metric){
    # initialize output
    out_frame <- tibble(site_code = as.character(), water_year = as.integer(), n = as.integer())

    # loop through sites
    for(i in unique(data_in$site_code)){
        target_site <- i

        site_data <- data_in %>%
            filter(site_code == target_site)

        interest <- site_data %>%
            filter(var == metric)

        years <- sort(unique(interest$water_year))
        # https://stackoverflow.com/questions/26639110/find-longest-consecutive-number-in-r
        s <- split(years, cumsum(c(TRUE, diff(years) != 1)))
        out_years <- s[[which.max(lengths(s))]]

        out_data <- site_data %>%
            filter(water_year %in% out_years)

        out_frame <- rbind(out_frame, out_data) %>%
            distinct()
    }
    return(out_frame)
}


## TRENDS #####

# df_in needs to be a long dataframe with site, water_year, var, and val
# outputs a
detect_trends <- function(df_in, diag_string){
    com_long <- df_in
    #  make trend dataset for entire prisim record ####
    out_frame <- tibble(site_code = as.character(),
                        var = as.character(),
                        start = as.integer(),
                        end = as.integer(),
                        trend = as.integer(),
                        p = as.integer())

    for(i in unique(com_long$site_code)) {

        target_site <- filter(com_long, site_code == i)

        dat_check <- na.omit(target_site)

        if(nrow(dat_check) > 1){
            #loop  through metrics
            for(j in unique(target_site$var)){

                target_solute <- filter(target_site, var == j)  %>%
                    arrange(water_year) %>%
                    na.omit()

                if(nrow(target_solute) > 9){
                    start <- min(target_solute$water_year)
                    end <- max(target_solute$water_year)
                    n <- nrow(target_solute)
                    test <- sens.slope(target_solute$val)
                    trend <- test[[1]]
                    p <- test[[3]]


                    inner <- tibble(site_code = i,
                                    var = j,
                                    start = start,
                                    end = end,
                                    n = n,
                                    trend = trend,
                                    p = p
                    )
                    out_frame <- rbind(out_frame, inner)

                    diag <- ggplot(target_solute, aes(x = water_year, y = val)) +
                        labs(title = paste0(i, ' ', j),
                             caption = paste0('n = ', n))+
                        geom_point()+
                        theme_few()

                    quietly(ggsave(plot = diag, filename = here('data_working', 'diag_plots', diag_string, i, paste0(i,'_',j,'.png')),
                                   create.dir = T))


                }else{inner <- tibble(site_code = i,
                                      var = j,
                                      start = NA,
                                      end = NA,
                                      n = NA,
                                      trend = NA,
                                      p = NA)
                out_frame <- rbind(out_frame, inner)} #solute level data avail check
            }# end solute loop
        }else{next} # site level data avail check
    } #end site loop
out_frame
} #end function

# make function to add flags to trend data
# needs a p-value in column called 'p'
# needs slope in column called 'trend'
add_flags <- function(data_in){
    data_out <- data_in %>%
        filter(
            #p > 0.05, # sig trends only
            var != 'a_flow_sig',
            var != 'b_flow_sig') %>%
        mutate(flag = case_when(p >= 0.05 ~ 'non-significant',
                                p < 0.05 & trend > 0 ~ 'increasing',
                                p < 0.05 & trend < 0 ~ 'decreasing',
                                p <0.05 & trend == 0 ~ 'flat')) %>%
        filter(!is.nan(flag),
               !is.na(flag))
    return(data_out)
}
