# clear environment
#rm(list = ls())
# Load packages.
library(here)
source(here('src', 'setup.R'))
#source(here('src', 'mega_zipper_data.R'))
flag_colors <- c('increasing' = "red", 'decreasing' = 'blue', 'flat' = 'green', 'non-significant' = "grey")

# read in data####
full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
    add_flags()
longest_run_trends <- read_csv(here('data_working', 'trends', 'best_run_prisim.csv')) %>%
    add_flags()

# check for weirdness
check1 <- full_prism_trends %>%
    filter(var == 'temp_mean_ann') %>%
    select(site_code, full_flag = flag)
unique(check1$full_flag)

check2 <- longest_run_trends %>%
    filter(var == 'temp_mean_ann') %>%
    select(site_code, lr_flag = flag)
unique(check2$lr_flag)

# check for sites that have a don't have a full prism trend, but do have a shorter one (impossible)
full_join(check1, check2) %>%
    filter(is.na(full_flag),
           !is.na(lr_flag))

# make sort order for coverage plot
sort_order <- read_csv(here('data_working', 'all_possible_good_siteyears.csv')) %>%
    group_by(site_code) %>%
    summarize(n = n()) %>%
    arrange(-n) %>%
    mutate(site_code=factor(site_code, levels=site_code))

# make q plot data from the full site_year dataset
full_data <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS'))
q_plot_data <-  full_data %>%
    select(site_code, water_year, q_mean) %>%
    drop_na(q_mean) %>%
    full_join(sort_order, ., by = 'site_code')

# make full, non-site climate data record ####
full_prisim <- full_prism_trends %>%
    select(site_code, n , var, trend, p, flag)

full_prisim %>%
    filter(is.na(flag)) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    select(site_code, domain)

# make longest run of site data during prisim data record
longest_run_prisim <- longest_run_trends %>%
    select(site_code, n, var, trend, p, flag)

longest_run_prisim %>%
    filter(is.na(flag)) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    distinct()%>%
    select(site_code, domain)

# make side panel table
fp_wide <- full_prisim %>%
    mutate(var = paste0(var,'_full')) %>%
    pivot_wider(id_cols = 'site_code', names_from = 'var', values_from = 'flag')

lrp_wide <- longest_run_prisim %>%
    mutate(var = paste0(var,'_longest_run')) %>%
    pivot_wider(id_cols = 'site_code', names_from = 'var', values_from = 'flag')

side_data <- full_join(fp_wide, lrp_wide) %>%
    full_join(distinct(select(q_plot_data, site_code, n)), .) %>%
    drop_na(n)

# data checks
side_data%>%
    filter(is.na(temp_mean_ann_full),
           !is.na(temp_mean_ann_longest_run))

side_data %>%
    filter(n > 9,
           is.na(q_mean_longest_run))

side_data%>%
    filter(is.na(temp_mean_ann_full),
           !is.na(temp_mean_ann_longest_run))

# plots #####
make_trend_panel <- function(target_trend, title_string){

    side_data %>%
        select(site_code, n, matches(target_trend)) %>%
    ggplot(aes(y = reorder(site_code, n), fill = .[[3]]))+
        geom_bar(width = 1)+
        coord_cartesian(xlim = c(0.4, .6))+
        scale_fill_manual(values = flag_colors, na.value = 'black')+#,
                          #labels = c('Decreasing', 'Increasing', 'Non-significant', 'Insufficient Data'))+
        theme_few()+
        theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none')+
        labs(title = paste0(title_string))
}

add_legend <- function(plot){
    plot+
        theme(legend.position = 'right')+
        labs(fill = 'Trend')
}
## make coverage plot ####

contrast_color <- 'darkorange'

c_master <- q_plot_data %>%
    ggplot(., aes(x = water_year, y = reorder(site_code, n)))+
    geom_text(aes(label = "-"), size = 8, family = "mono")+
    theme_few()+
    theme(legend.position = 'none',
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks.y = element_blank())+
    #scale_color_viridis(discrete = T) +
    annotate("text", x=modis_year-1, y=25, label="MODIS", angle=90, size=5, color=contrast_color)+
    geom_vline(xintercept = modis_year, color = contrast_color)+
    annotate("text", x=landsat_year-1, y=25, label="LANDSAT-5", angle=90, size=5, color=contrast_color)+
    geom_vline(xintercept = landsat_year, color = contrast_color)+
    annotate("text", x=prisim_year-1, y=25, label="PRISM", angle=90, size=5, color=contrast_color)+
    geom_vline(xintercept = prisim_year, color = contrast_color)+
    labs(title = 'MS Site Data')
c_master


## assemble plot #####
zipper_plot <- make_trend_panel('temp_mean_ann_full', 'Ta') +
    make_trend_panel('precip_mean_ann_full', 'P')+
    make_trend_panel('gpp_conus_full', 'GPP')+
    c_master +
    make_trend_panel('temp_mean_ann_longest_run', 'Ta') +
    make_trend_panel('precip_mean_ann_longest_run', 'P') +
    make_trend_panel('gpp_conus_longest_run', 'GPP')+
    make_trend_panel('stream_temp_mean_ann_longest_run', 'Ts') +
    make_trend_panel('q_mean_longest_run', 'Q') +
    make_trend_panel('q_rbi_longest_run', 'RBI') +
    add_legend(make_trend_panel('q_ar1_longest_run', 'AR1')) +
    plot_layout(ncol = 11, widths = c(.25, .25, .25, 5, .25, .25, .25, .25, .25, .25, .25))
zipper_plot

# why are there black bands in our data?
na_flag_sites <- full_prisim %>% filter(is.na(flag)) %>% select(site_code) %>% distinct()

test_tbl <- ms_site_data %>%
    filter(site_code %in% na_flag_sites$site_code)

test_tbl %>%
    st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
    mapview()

# Let's check to see which sites display NAs in air temp
# and precip trends in total, first, since this shouldn't occur
# at most sites.
na_full_precip_sites <- side_data %>%
    filter(is.na(precip_mean_ann_full)) %>%
    left_join(ms_site_data)
# Ok, so all these sites are in Puerto Rico, Alaska, & Sweden

na_full_temp_sites <- side_data %>%
    filter(is.na(temp_mean_ann_full)) %>%
    left_join(ms_site_data)
# Same domains here.

na_longest_q_sites <- side_data %>%
    filter(is.na(q_mean_longest_run)) %>%
    left_join(ms_site_data)

# # add domain to this tomorrow
# ggplotly(c_master, tooltip = 'all')
ggplotly(c_master)


side_data %>%
    select(site_code, temp_mean_ann_longest_run) %>%
    drop_na(temp_mean_ann_longest_run) %>%
    left_join(., ms_site_data, by ='site_code') %>%
    st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
    mapview()

test <- side_data %>%
    select(site_code, temp_mean_ann_longest_run) %>%
    drop_na(temp_mean_ann_longest_run) %>%
    left_join(., ms_site_data, by ='site_code')

length(unique(test$site_code))
