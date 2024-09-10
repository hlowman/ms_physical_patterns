# handle setup
library(here)
source(here('src', 'setup.R'))
source(here('src', 'set_master_coverage_vars.R'))

# 0 Read in q data ####
# load in q data and remove duplicates
q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F) %>%
    mutate(water_year = as.integer(as.character(water_year(datetime, origin = 'usgs')))) %>%
    distinct(., site_code, datetime, .keep_all = TRUE) %>%
    filter(ms_interp == 0)

# 1 Data frequency check ####
# only want sites that are reporting q at 4 times a week
# note this spits out number of weeks in a given water year
freq_check <- q_data %>%
    mutate(week_year = paste0(week(datetime), '_', water_year)) %>%
    group_by(site_code, week_year) %>%
    summarize(water_year = max(water_year),
              n = n()) %>%
    filter(n >= minimum_per_week_sampling_frequency_master) %>%
    group_by(site_code, water_year) %>%
    summarize(n = n())

# make tibble of good site years for data frequency ####
good_site_year_freq <- freq_check %>%
    filter(n >= good_weeks_to_year_master) %>%
    select(site_code, water_year)

# create and save final tibble of site codes and years
saveRDS(good_site_year_freq, here('data_working', 'good_zipper_plot_years.RDS'))

# trend detection ####
# making a function for this
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


        }else{next} #solute level data avail check
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

# adding if statement to make rerunning easier
# if file exisits it will print the file creation time
# delete file or rerun this chunk manually to make a new one
if(!file.exists(here('data_working', 'climate_trends_full_prisim.csv'))){
# create full prisim trends data #####
clim <- readRDS(file = here('data_working', 'clim_summaries.rds')) %>%
    select(-contains('date')) %>%
    pivot_longer(cols = -c(site_code, water_year), names_to = 'var', values_to = 'val')

# run trend analysis
clim_trends <- detect_trends(clim, 'full_prisim')

# save trend analysis
clim_trends %>%
    add_flags()%>%
    write_csv(.,file = here('data_working', 'climate_trends_full_prisim.csv'))
}else(print(file.info(here('data_working', 'climate_trends_full_prisim.csv'))$ctime))

# create longest run w/ prisim data ####
# start with site years that passed freq check
good_site_years_fp <- good_site_year_freq %>%
    filter(water_year >= prisim_year)
# initialize output
out_frame <- tibble(site_code = as.character(), water_year = as.integer(), n = as.integer())

# loop through sites
for(i in unique(good_site_years_fp$site_code)){
    target_site <- i

    site_data <- good_site_years_fp %>%
        filter(site_code == target_site)

    years <- site_data$water_year
    # https://stackoverflow.com/questions/26639110/find-longest-consecutive-number-in-r
    s <- split(years, cumsum(c(TRUE, diff(years) != 1)))
    out_years <- s[[which.max(lengths(s))]]

    out_data <- site_data %>%
        filter(water_year %in% out_years)

    out_frame <- rbind(out_frame, out_data)
}

saveRDS(out_frame, here('data_working', 'longest_run_prisim_covered_site_years.RDS'))
# filter full dataset to longest runs during prisim
prisim_data <- readRDS(here('data_working', 'discharge_metrics_siteyear_prisim.rds')) %>%
    select(-contains('date')) %>%
    pivot_longer(cols = -c('site_code', 'water_year'), names_to = 'var', values_to = 'val')

# do trend analysis
prisim_trends <- detect_trends(prisim_data, 'prisim_longest_site_run') %>%
    add_flags()

# save results out
saveRDS(prisim_trends, here('data_working', 'longest_run_prisim_covered_trends.RDS'))
