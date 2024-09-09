# Load packages.
library(here)
source(here('src', 'setup.R'))
if(file.exists(here('data_working', 'good_zipper_plot_years.RDS')) == F){
    source(here('src', 'q_mega_zipper_data.R'))
}else(print(file.info(here('data_working', 'good_zipper_plot_years.RDS'))$ctime))

# coverage plot ####
## read in data from coverage check script ####
sort_order <- readRDS(here('data_working', 'good_zipper_plot_years.RDS')) %>%
    group_by(site_code) %>%
    summarize(n = n()) %>%
    arrange(-n) %>%
    mutate(site_code=factor(site_code, levels=site_code))

plot_data <- right_join(sort_order, readRDS(here('data_working', 'good_zipper_plot_years.RDS')), by = 'site_code') %>%
    left_join(., ms_site_data, by = 'site_code')
# make full, non-site climate data record ####


## make coverage plot ####
prisim_year <- 1980
landsat_year <- 1984
modis_year <- 2000

c_master <- ggplot(plot_data, aes(x = water_year, y = reorder(site_code, n), color = domain))+
    geom_text(aes(label = "-"), size = 15, family = "mono") +
    theme_few()+
    theme(legend.position = 'none',
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())+
    scale_color_viridis(discrete = T) +
    annotate("text", x=modis_year-1, y=25, label="MODIS", angle=90, size=5, color="red")+
    geom_vline(xintercept = modis_year, color = 'red')+
    annotate("text", x=landsat_year-1, y=25, label="LANDSAT-5", angle=90, size=5, color="red")+
    geom_vline(xintercept = landsat_year, color = 'red')+
    annotate("text", x=prisim_year-1, y=25, label="PRISM", angle=90, size=5, color="red")+
    geom_vline(xintercept = prisim_year, color = 'red')+
    labs(title = 'Data Coverage')
c_master


## read in trend data ####
trends_flagged <- read_csv(here('data_working', 'hydro_climate_trends_flagged.csv'))
flag_colors <- c('increasing' = "orange", 'decreasing' = "purple", 'flat' = 'green', 'non-significant' = "cyan1")


make_modis_trend_panel <- function(target_trend, title_string){
    plot_data %>%
        select(site_code, n) %>%
        distinct() %>%
        left_join(.,trends_flagged, by = 'site_code') %>%
        filter(var %in% c(target_trend, NA)) %>%
    ggplot(aes(y = reorder(site_code, n.x), fill = flag))+
        geom_bar(width = .4)+
        coord_cartesian(xlim = c(0.4, .6))+
        scale_fill_manual(values = flag_colors)+
        theme_few()+
        theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none')+
        labs(title = paste0(title_string))
}


c_master + make_modis_trend_panel('m1_meanq', 'Q') +
    make_modis_trend_panel('rbiq', 'RBI') +
    make_modis_trend_panel('m5_ar1q', 'AR1') +
    make_modis_trend_panel('precip_mean_ann', 'P') +
    make_modis_trend_panel('temp_mean_ann', 'T')+
    plot_layout(ncol = 6, widths = c(5, .25, .25, .25, .25, .25))
