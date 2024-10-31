# clear environment
#rm(list = ls())
# Load packages.
library(here)
source(here('src', 'setup.R'))

ms_groups <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv')) %>%
    select(site_code, grouping) %>%
    mutate(source = 'ms')
camels_groups <- read_csv(here('data_working', 'camels_groups.csv'))%>%
    select(site_code, grouping) %>%
    mutate(source = 'camels')
grid_groups <- read_csv(here('data_working', 'grid_groups.csv'))%>%
    select(site_code, grouping) %>%
    mutate(source = 'grid')

groups <- rbind(ms_groups, camels_groups) %>%
    rbind(., grid_groups)

ggplot(groups, aes(x= grouping)) +
    geom_bar()+
    facet_wrap(~source,
               ncol = 1, scales = 'free_y')+
    theme_few()
