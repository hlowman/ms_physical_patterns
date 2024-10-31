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
#library(ggplotify)
#library(gt)
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
library(logger)
library(foreach)

# Using revised code from Mike to point to new data.
rdata_path <- "data_raw/ms" # updated path

# dl initial dataset
# options(timeout = 9999)
# macrosheds::ms_download_core_data(
#     macrosheds_root = rdata_path,
#     domains = 'all'
# )
# macrosheds::ms_download_ws_attr(
#     macrosheds_root = rdata_path,
#     dataset = 'all'
# )

ms_site_data <- ms_load_sites()
ms_ws_attr <- read_feather(file.path(rdata_path, 'v2', 'watershed_summaries.feather'))

nv <- as.environment('package:macrosheds')

for(ms_data in c('ms_vars_ts', 'ms_vars_ws', 'ms_site_data', 'ms_var_catalog')){
    unlockBinding(ms_data, nv)
    assign(ms_data, get(ms_data), envir = nv)
    lockBinding(ms_data, nv)
}

# Set directory to folder containing new data.
my_ms_dir <- "data_raw/ms"

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
# set logger for script
set_logger <- function(){
    log_file_name <- paste0(tools::file_path_sans_ext(basename(rstudioapi::getSourceEditorContext()$path)),
                            '_', format(Sys.time(), '%D_%H_%M_%S'), '.txt') %>%
        gsub('/','',.)
    log_file_path <- file.path('log', log_file_name)
    file.create(log_file_path)

    log_appender(appender_file(log_file_path))
}


## CARPENTRY ####

# freq_check needs a data frame of long format q_data with water_year
frequency_check <- function(data_in){

    q_data <- data_in

    freq_check <- q_data %>%
    mutate(week_year = paste0(week(date), '_', water_year)) %>%
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
            filter(var == metric) %>%
            select(water_year, val) %>%
            drop_na(val)

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

# reduces long data frame to years of data with at least 60% coverage
reduce_to_best_range <- function(data_in, metric, max_missing = 0.4) {

    output <- head(data_in, n =0)

    max_missing = max_missing
    target_metric = metric

for (i in unique(data_in$site_code)){
    target_site = i
    data <- data_in %>%
        filter(site_code == target_site,
               var == target_metric) %>%
        distinct() %>%
        na.omit()

    if(nrow(data) > 9){

    year_seq <- seq(min(data$water_year), max(data$water_year))
    df <- tibble(water_year = year_seq, is_present = year_seq %in% data$water_year)%>%
        mutate(group = cumsum(lag(is_present == FALSE, default = FALSE)))


    # Initialize variables to track the best range
    best_start <- NA
    best_end <- NA
    longest_length <- 0

    # Loop through all possible starting and ending points
    for (start_idx in seq_along(df$water_year)) {
        for (end_idx in start_idx:length(df$water_year)) {

            # Extract the subrange of years
            subrange <- df[start_idx:end_idx, ]

            # Calculate the percentage of missing years
            total_years <- nrow(subrange)
            missing_years <- sum(!subrange$is_present)
            missing_pct <- missing_years / total_years

            # If the missing percentage is below the threshold and the range is longer, update
            if (missing_pct <= max_missing && total_years > longest_length) {
                longest_length <- total_years
                best_start <- subrange$water_year[1]
                best_end <- subrange$water_year[nrow(subrange)]
            } # end if
        } # end start index
    } # end end index
    inner <- data_in %>%
        filter(site_code == target_site,
               water_year %in% best_start:best_end)

    output <- rbind(output, inner)

}else{
inner <- head(data) %>%
    add_case(site_code = target_site)

output <- rbind(output, inner)
} # end 10 year data check
} # end for loop for sites
return(output)
}   # end function
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
                        p = as.integer(),
                        code = as.character())

    #foreach(i = unique(com_long$site_code)) %do%{
    for(i in unique(com_long$site_code)){
        target_site <- filter(com_long, site_code == i)

        dat_check <- na.omit(target_site)

        if(nrow(dat_check) > 1){
            #loop  through metrics
            #foreach(j = unique(target_site$var)) %do%{
            for(j in unique(target_site$var)){
                target_solute <- filter(target_site, var == j)  %>%
                    arrange(water_year) %>%
                    distinct() %>%
                    na.omit()

                if(nrow(target_solute) > 9 ){

                    start <- min(target_solute$water_year)
                    end <- max(target_solute$water_year)
                    n <- nrow(target_solute)

                    # check_vec <- (target_solute$water_year-lag(target_solute$water_year))[-1]
                    #
                    # if(all(check_vec == 1)){
                    slope_data <- target_solute %>%
                        #full_join(., tibble(water_year = start:end)) %>%
                        select(val) %>%
                        as.ts()
                    rownames(slope_data) <- target_solute$water_year
                    test <- sens.slope(slope_data)
                    trend <- test[[1]]
                    p <- test[[3]]


                    inner <- tibble(site_code = i,
                                    var = j,
                                    start = start,
                                    end = end,
                                    n = n,
                                    trend = trend,
                                    p = p,
                                    code = 'good'
                    )
                    # bind out
                    out_frame <- rbind(out_frame, inner)

                    # diag <- ggplot(target_solute, aes(x = water_year, y = val)) +
                    #     labs(title = paste0(i, ' ', j),
                    #          caption = paste0('n = ', n))+
                    #     geom_point()+
                    #     theme_few()
                    #
                    # quietly(ggsave(plot = diag, filename = here('data_working', 'diag_plots', diag_string, i, paste0(i,'_',j,'.png')),
                    #                create.dir = T, width = 7, height = 7))
                    # }else{
                    #
                    #     gap_starts <- paste0((target_solute$water_year[which(check_vec != 1)]), collapse = ',')
                    #
                    #     inner <- tibble(site_code = i,
                    #                       var = j,
                    #                       start = start,
                    #                       end = end,
                    #                       n = n,
                    #                       trend = NA,
                    #                       p = NA,
                    #                       code = paste0('gaps_at_', gap_starts))
                    #     # bind out
                    #     out_frame <- rbind(out_frame, inner)
                    #     }


                }else{inner <- tibble(site_code = i,
                                      var = j,
                                      start = NA,
                                      end = NA,
                                      n = NA,
                                      trend = NA,
                                      p = NA,
                                      code = 'under_10')
                # bind out
                out_frame <- rbind(out_frame, inner)
                } #solute level data avail check
            }# end solute loop
            }else{next} # site level data avail check
    } #end site loop
return(out_frame)
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
