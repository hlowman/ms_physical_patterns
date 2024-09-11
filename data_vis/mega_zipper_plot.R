# clear environment
rm(list = ls())
# Load packages.
library(here)
source(here('src', 'setup.R'))
#source(here('src', 'q_mega_zipper_data.R'))

# read in data####
full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
    add_flags()
longest_run_trends <- read_csv(here('data_working', 'trends', 'longest_site_run_prisim.csv')) %>%
    add_flags()
flag_colors <- c('increasing' = "red", 'decreasing' = 'blue', 'flat' = 'green', 'non-significant' = "grey")

# make sort order for coverage plot
sort_order <- read_csv(here('data_working', 'all_possible_good_siteyears.csv')) %>%
    group_by(site_code) %>%
    summarize(n = n()) %>%
    arrange(-n) %>%
    mutate(site_code=factor(site_code, levels=site_code))

# make q plot data from the full site_year dataset
full_data <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS'))
q_plot_data <-  full_data %>%
    select(site_code, water_year, m1_meanq) %>%
    drop_na() %>%
    full_join(sort_order, ., by = 'site_code')

# make full, non-site climate data record ####
full_prisim <- q_plot_data %>%
    select(site_code, n) %>%
    distinct() %>%
    left_join(., full_prism_trends, by = 'site_code') %>%
    select(site_code, n = n.x, var, trend, p, flag)%>%
    full_join(., sort_order, by = 'site_code') %>%
    rename(n = n.x)

full_prisim %>%
    filter(is.na(flag)) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    select(site_code, domain) %>%
    distinct()

# make longest run of site data during prisim data record
longest_run_prisim <- q_plot_data %>%
    select(site_code, n) %>%
    distinct() %>%
    right_join(., longest_run_trends, by = 'site_code') %>%
    select(site_code, n = n.x, var, trend, p, flag)%>%
    filter(n >= 10) %>%
    full_join(sort_order,. , by = 'site_code')%>%
    rename(n = n.x)

longest_run_prisim %>%
    filter(is.na(flag)) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    select(site_code, domain) %>%
    distinct()


# plots #####
make_trend_panel <- function(data_in, target_trend, title_string){
    plot_data <- data_in

    plot_data %>%
        filter(var %in% c(target_trend, NA)) %>%
        distinct()%>%
    ggplot(aes(y = reorder(site_code, n), fill = flag))+
        geom_bar(width = 1)+
        coord_cartesian(xlim = c(0.4, .6))+
        scale_fill_manual(values = flag_colors, na.value = 'black',
                          labels = c('Decreasing', 'Increasing', 'Non-significant', 'Insufficient Data'))+
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
prisim_year <- 1980
landsat_year <- 1984
modis_year <- 2000

contrast_color <- 'darkorange'

c_master <- q_plot_data %>%
    ggplot(., aes(x = water_year, y = reorder(site_code, n)))+
    geom_text(aes(label = "-"), size = 15, family = "mono")+
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
zipper_plot <- make_trend_panel(full_prisim, 'temp_mean_ann', 'Ta') +
    make_trend_panel(full_prisim, 'precip_mean_ann', 'P')+
    c_master +
    make_trend_panel(longest_run_prisim, 'temp_mean_ann', 'Ta') +
    make_trend_panel(longest_run_prisim, 'precip_mean_ann', 'P') +
    make_trend_panel(longest_run_prisim, 'm1_meanq', 'Q') +
    make_trend_panel(longest_run_prisim, 'rbiq', 'RBI') +
    add_legend(make_trend_panel(longest_run_prisim, 'm5_ar1q', 'AR1')) +
    plot_layout(ncol = 8, widths = c(.25, .25, 5, .25, .25, .25, .25, .25))
zipper_plot

# why are there black bands in our data?
na_flag_sites <- full_prisim %>% filter(is.na(flag)) %>% select(site_code) %>% distinct()

test_tbl <- ms_site_data %>%
    filter(site_code %in% na_flag_sites$site_code)

test_tbl %>%
    st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
    mapview()


# # add domain to this tomorrow
# ggplotly(c_master, tooltip = 'all')
ggplotly(make_trend_panel(full_prisim, 'temp_mean_ann', 'Ta'))
