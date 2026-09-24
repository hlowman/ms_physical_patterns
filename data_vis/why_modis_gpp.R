# Why the analysis uses MODIS rather than Landsat GPP:
# (a) cross-site Landsat GPP shifts level at sensor transitions while MODIS does not;
# (b) on identical sites and years, Landsat flags far more greening than MODIS.
library(here)
source(here('src', 'setup.R'))
library(patchwork)

sites <- unique(read_csv(here('data_working', 'trends', 'full_prisim_climate.csv'), show_col_types = FALSE)$site_code)
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds')) %>%
    distinct() %>%
    filter(agg_code == 'annual', site_code %in% sites) %>%
    select(site_code, water_year, landsat = gpp_CONUS_30m_median, modis = gpp_global_500m_median)

prod_cols <- c('Landsat (NTSG, 30 m)' = '#D55E00', 'MODIS (MOD17A3HGF, 500 m)' = '#0072B2')
ref_window <- 2001:2021
edge_years <- c(1986, 2022)   # partial composite years at the ends of the Landsat record

# ---- (a) site-standardized annual series ----
z <- metrics %>%
    group_by(site_code) %>%
    mutate(landsat_z = (landsat - mean(landsat[water_year %in% ref_window], na.rm = TRUE)) /
                       sd(landsat[water_year %in% ref_window], na.rm = TRUE),
           modis_z = (modis - mean(modis[water_year %in% ref_window], na.rm = TRUE)) /
                     sd(modis[water_year %in% ref_window], na.rm = TRUE)) %>%
    ungroup() %>%
    select(site_code, water_year, `Landsat (NTSG, 30 m)` = landsat_z, `MODIS (MOD17A3HGF, 500 m)` = modis_z) %>%
    pivot_longer(-c(site_code, water_year), names_to = 'product', values_to = 'z') %>%
    filter(!is.na(z), !(product == 'Landsat (NTSG, 30 m)' & water_year %in% edge_years))

yearly <- z %>%
    group_by(product, water_year) %>%
    summarize(med = median(z), q25 = quantile(z, .25), q75 = quantile(z, .75), n = n(), .groups = 'drop')

eras <- tribble(
    ~product,                     ~start, ~end,  ~label,
    'Landsat (NTSG, 30 m)',       1987,   1998,  'Landsat 5 only',
    'Landsat (NTSG, 30 m)',       1999,   2012,  'Landsat 5 + 7',
    'Landsat (NTSG, 30 m)',       2013,   2021,  'Landsat 7 + 8',
    'MODIS (MOD17A3HGF, 500 m)',  2001,   2012,  'Terra MODIS',
    'MODIS (MOD17A3HGF, 500 m)',  2013,   2023,  'Terra MODIS'
) %>%
    rowwise() %>%
    mutate(era_mean = mean(yearly$med[yearly$product == product & yearly$water_year >= start & yearly$water_year <= end])) %>%
    ungroup()

events <- tribble(
    ~year, ~label,
    1999,  'Landsat 7 launched',
    2003,  'Landsat 7 SLC failure',
    2011,  'Landsat 5 retired',
    2013,  'Landsat 8 launched'
)

cat('=== Landsat era means (cross-site median z, reference 2001-2021) ===\n')
print(as.data.frame(eras %>% mutate(era_mean = round(era_mean, 2))))

p_a <- ggplot() +
    geom_vline(data = events, aes(xintercept = year), linetype = 'dashed', color = 'grey55', linewidth = 0.4) +
    geom_text(data = events, aes(x = year, y = 2.35, label = label), angle = 90, hjust = 1, vjust = -0.4,
              size = 3, color = 'grey35') +
    geom_ribbon(data = yearly, aes(x = water_year, ymin = q25, ymax = q75, fill = product), alpha = 0.18) +
    geom_line(data = yearly, aes(x = water_year, y = med, color = product), linewidth = 0.6) +
    geom_point(data = yearly, aes(x = water_year, y = med, color = product), size = 1.6) +
    geom_segment(data = eras, aes(x = start - 0.4, xend = end + 0.4, y = era_mean, yend = era_mean, color = product),
                 linewidth = 1.6, alpha = 0.9, lineend = 'round') +
    geom_hline(yintercept = 0, color = 'grey70', linewidth = 0.3) +
    scale_color_manual(values = prod_cols, name = NULL) +
    scale_fill_manual(values = prod_cols, name = NULL) +
    scale_x_continuous(breaks = seq(1985, 2025, 5)) +
    coord_cartesian(ylim = c(-3, 2.4)) +
    theme_few(base_size = 12) +
    theme(legend.position = c(0.17, 0.9), legend.background = element_rect(fill = 'white', color = NA)) +
    labs(title = '(a) Landsat GPP shifts level at sensor transitions; single-sensor MODIS does not',
         subtitle = paste0('Cross-site median of site-standardized annual GPP (', length(sites),
                           ' MacroSheds sites); bands = interquartile range; thick bars = mean per sensor era'),
         x = 'Water year', y = 'GPP anomaly (SD units; each site scaled on 2001–2021)')

# ---- (b) same sites, same years: trend flags by product ----
flag_levels <- c('increasing', 'non-significant', 'decreasing', 'insufficient data')
flag_cols <- c('increasing' = 'green4', 'decreasing' = 'brown4',
               'non-significant' = 'grey', 'insufficient data' = 'black')

window_flags <- function(col, start, end) {
    metrics %>%
        select(site_code, water_year, val = all_of(col)) %>%
        filter(water_year >= start, water_year <= end) %>%
        drop_na(val) %>%
        mutate(var = col) %>%
        select(site_code, water_year, var, val) %>%
        detect_trends() %>%
        add_flags() %>%
        select(site_code, flag)
}
flags <- tibble(site_code = sites) %>%
    left_join(rename(window_flags('landsat', 2001, 2021), `Landsat` = flag), by = 'site_code') %>%
    left_join(rename(window_flags('modis', 2001, 2021), `MODIS` = flag), by = 'site_code') %>%
    mutate(across(-site_code, ~ factor(replace_na(.x, 'insufficient data'), levels = flag_levels)))

counts <- flags %>%
    pivot_longer(-site_code, names_to = 'product', values_to = 'flag') %>%
    count(product, flag) %>%
    mutate(product = factor(product, levels = c('Landsat', 'MODIS')))
cat('\n=== Trend flags, 2001-2021, same 166 sites ===\n')
print(pivot_wider(counts, names_from = product, values_from = n, values_fill = 0))

p_b <- ggplot(counts, aes(x = product, y = n, fill = flag)) +
    geom_col(width = 0.65) +
    geom_text(data = filter(counts, n >= 12), aes(label = n), position = position_stack(vjust = 0.5),
              color = 'white', size = 3.5, fontface = 'bold') +
    scale_fill_manual(values = flag_cols, breaks = flag_levels, name = 'GPP trend\n(Sen\'s slope, 95% CI)') +
    theme_few(base_size = 12) +
    theme(legend.position = 'right') +
    labs(title = '(b) Same sites, same years (2001–2021)',
         subtitle = 'Sites flagged as greening: Landsat 85, MODIS 16',
         x = NULL, y = 'Sites')

fig <- p_a + p_b + plot_layout(widths = c(2.4, 1.15)) &
    theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 9.5))

ggsave(here('figures', 'why_modis_gpp.png'), fig, width = 13, height = 5.2, dpi = 300)
cat('\nFigure saved to figures/why_modis_gpp.png\n')
