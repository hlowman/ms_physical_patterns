# handle setup
library(here)
source(here('src', 'setup.R'))

camels_metrics <- read_csv(here('data_raw', 'camels_rs', 'et.csv')) %>%
    full_join(.,  read_csv(here('data_raw', 'camels_rs', 'gpp.csv')), by = c('hru_id', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'camels_rs', 'precip_temp.csv')), by = c('hru_id', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'camels_rs', 'aridity.csv')), by = c('hru_id', 'year')) %>%
    select(site_code = hru_id, water_year = year, ET, GPP, ppt, tmean, pdsi, spei30d) %>%
    pivot_longer(cols = -c('water_year', 'site_code'), names_to = 'var', values_to = 'val')

camels_trends <- camels_metrics %>%
    detect_trends() %>%
    add_flags()

#write_csv(camels_trends, here('data_working', 'camels_trends.csv'))
#camels_trends <- read_csv(here('data_working', 'camels_trends.csv'))

ggplot(camels_trends, aes(x = flag)) +
    geom_bar() +
    facet_wrap(~var, ncol = 1) +
    theme_few()

# groupings ####

camels_groups <- camels_trends %>%
    select(site_code, var, trend, p, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, p), names_from = var) %>%
    na.omit() %>%
    mutate(wetting = case_when(trend_ppt > 0 & p_ppt <= 0.05 ~ 'W',
                               trend_ppt < 0 & p_ppt <= 0.05 ~ 'D',
                               p_ppt > 0.05 ~ 'N'),
           warming = case_when(trend_tmean > 0 & p_tmean <= 0.05 ~ 'H',
                               trend_tmean < 0 & p_tmean <= 0.05 ~ 'C',
                               p_tmean > 0.05 ~ 'N'),
           greening = case_when(trend_GPP > 0 & p_GPP <= 0.05 ~ 'G',
                                trend_GPP < 0 & p_GPP <= 0.05 ~ 'B',
                                p_GPP > 0.05 ~ 'N',
                                trend_GPP == 0 ~ 'N'),
           grouping = as.factor(paste0(warming, wetting, greening))
    )

camels_groups$grouping <- factor(camels_groups$grouping, levels = c('HDG', #strong down
                                                                'HDN', 'HNG', 'NDG', # mid down
                                                                'HDB', 'HNN', 'NNG', 'NDN', # light down
                                                                'NNN', # no change
                                                                'HWG', 'HWN', 'HNB', 'NDB', 'NWG', 'HWB', 'CNG', 'CWG', # variable
                                                                'CNN', 'NNB', 'NWN', #light up
                                                                'NWB')) # strong up

# write_csv(camels_groups, here('data_working', 'camels_groups.csv'))
# camels_groups <- read_csv(here('data_working', 'camels_groups.csv'))

ggplot(camels_groups, aes(x = grouping))+
    geom_bar()+
    theme_few(base_size = 20)+
    scale_fill_manual(values = flag_colors)
