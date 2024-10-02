# handle setup
library(here)
source(here('src', 'setup.R'))

# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS')) %>%
    distinct()

good_groups <- metrics %>%
    mutate(group_tag = case_when(water_year %in% 1984:1997 ~ 1,
                                 water_year %in% 1998:2011 ~ 2,
                                 water_year %in% 2012:2024~ 3)) %>%
    drop_na(group_tag) %>%
    select(!contains('date')) %>%
    pivot_longer(cols = -c(site_code, water_year, group_tag), names_to = 'var', values_to = 'val') %>%
    select(site_code, group_tag, var, val) %>%
    drop_na(val) %>%
    group_by(site_code, group_tag, var) %>%
    summarize(n = n()) %>%
    filter(n > 9)

has_pair <- good_groups %>%
    group_by(site_code, var) %>%
    distinct() %>%
    summarize(n = n()) %>%
    filter(n > 1)


binned_data <- metrics %>%
    mutate(group_tag = case_when(water_year %in% 1984:1997 ~ 1,
                                 water_year %in% 1998:2011 ~ 2,
                                 water_year %in% 2012:2024~ 3)) %>%
    select(!contains('date')) %>%
    pivot_longer(cols = -c(site_code, water_year, group_tag), names_to = 'var', values_to = 'val') %>%
    right_join(., good_groups, by = c('site_code', 'group_tag', 'var')) %>%
    right_join(., has_pair, by = c('site_code', 'var')) %>%
    group_by(site_code, group_tag, var) %>%
    #summarize(val = mean(val, na.rm = T)) %>%
    mutate(group_tag = as.factor(group_tag)) %>%
    drop_na(val)

out_table <- tibble(site_code = as.character(),
                    var = as.character(),
                    test = as.character(),
                    diff = as.numeric(),
                    p = as.numeric())

for(i in unique(binned_data$site_code)){
site_data <- binned_data %>%
    filter(site_code == i)

for(j in unique(site_data$var)){
        target_var = j

        data_inner <- site_data %>%
            filter(var == target_var) %>%
            na.omit()

        anova_test <- lm(val ~ as.factor(group_tag), data = data_inner)

        test <- TukeyHSD(aov(anova_test))

        names(test) <- paste0(i, '_', j)

        diffs <- test[[1]][,1]
        p <- test[[1]][,4]
        labels <- rownames(test[[1]])

        inner <- tibble(site_code = i,
               var = j,
                test = labels,
               diff = diffs,
               p_value = p
               )

        out_table <- rbind(out_table, inner)
    }
}

write_csv(out_table, file = here('data_working', 't_testing.csv'))

t_tests <- read_csv(here('data_working', 't_testing.csv'))


# air temp ####
# density
make_density_plot <- function(target_test, target_var){
t_tests %>%
    filter(#p_value < 0.05,
        test == target_test,
        var == target_var) %>%
    mutate(group = case_when(p_value > 0.05 | is.nan(p_value) ~ 'non_sig',
                             p_value <= 0.05 ~ 'sig')) %>%
    ggplot(aes(x = diff, fill = group)) +
        geom_density()+
    theme_few()+
    scale_fill_viridis(discrete = T)+
    labs(title = target_test )
}
target_var <- 'temp_mean_ann'

target_test <- "3-1"
make_density_plot(target_test, target_var)

target_test <- "3-2"
make_density_plot(target_test, target_var)

target_test <- "2-1"
make_density_plot(target_test, target_var)

# maps

make_map <- function(target_test, target_var){
t_tests %>%
    left_join(ms_site_data, by = 'site_code') %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    filter(#p_value < 0.05,
        test == target_test,
        var == target_var) %>%
    mutate(plot_diff = case_when(p_value > 0.5 ~ NA,
                                 p_value <= 0.05 ~ diff)) %>%
    mapview(zcol = 'plot_diff')
}

target_test <- "3-1"
make_map(target_test, target_var)

target_test <- "3-2"
make_map(target_test, target_var)

target_test <- "2-1"
make_map(target_test, target_var)


# gpp ####
# density
target_var <- 'q_mean'

target_test <- "3-1"
make_density_plot(target_test, target_var)

target_test <- "3-2"
make_density_plot(target_test, target_var)

target_test <- "2-1"
make_density_plot(target_test, target_var)

# maps
target_test <- "3-1"
t_tests %>%
    left_join(ms_site_data, by = 'site_code') %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    filter(#p_value < 0.05,
        test == target_test,
        var == target_var) %>%
    mapview()

target_test <- "3-2"
make_map(target_test, target_var)

target_test <- "2-1"
make_map(target_test, target_var)
