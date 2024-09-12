# Time Series Figures Script
# September 11, 2024
# Heili Lowman

#### READ ME ####

# The following script will be used to generate figures
# that display time-series of climate and in-stream data
# for each site at an annual time-step (to have as a raw
# data gut-check as we perform trend analyses).

#### Setup ####

# Load packages, setup wd, and load site metadata.
library(here)
source(here('src', 'setup.R'))

#### Data ####

# Load in annual data summaries.
data <- readRDS(here('data_working', 'discharge_metrics_siteyear_all.rds'))

# And join with site-level data.
data <- data %>%
    left_join(ms_site_data, by = c("site_code"))

#### Plot ####

# Test plot before functionalizing things.
data_reformat <- data %>%
    filter(site_code == "CARI") %>%
    select(domain, site_code, water_year,
           temp_mean_ann, precip_mean_ann,
           m1_meanq, rbiq, m5_ar1q) %>%
    rename(mean_q = m1_meanq,
           rbi_q = rbiq,
           ar1_q = m5_ar1q) %>%
    pivot_longer(temp_mean_ann:ar1_q,
                 names_to = "variable") %>%
    mutate(variable = factor(variable,
                             levels = c("temp_mean_ann", "precip_mean_ann",
                                        "mean_q", "rbi_q", "ar1_q")))

(covariate_fig <- ggplot(data_reformat,
                        aes(x = water_year,
                            y = value,
                            color = variable)) +
    geom_point(alpha = 0.9, size = 2) +
    scale_color_manual(values = c("#D46F10", "#69B9FA",
                                  "#59A3F8", "#4B8FF7", "#5A7ECB")) +
    labs(x = "Water Year (Oct 1 - Sept 30)",
         title = paste(data_reformat$domain[1],
                       data_reformat$site_code[1], sep = " ")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_blank()) +
    facet_grid(variable~., scales = "free"))

ggsave(covariate_fig,
       filename = "figures/test_cov_ts_fig.png",
       width = 20,
       height = 18,
       units = "cm")

# Now, to iterate a function over the full list of data.
site_sub_list <- split(data, data$site_code)

plotting_covar <- function(x) {

    df <- x %>%
        select(domain, site_code, water_year,
               temp_mean_ann, precip_mean_ann,
               m1_meanq, rbiq, m5_ar1q) %>%
        rename(mean_q = m1_meanq,
               rbi_q = rbiq,
               ar1_q = m5_ar1q) %>%
        pivot_longer(temp_mean_ann:ar1_q,
                     names_to = "variable") %>%
        mutate(variable = factor(variable,
                                 levels = c("temp_mean_ann", "precip_mean_ann",
                                            "mean_q", "rbi_q", "ar1_q")))
    p <- ggplot(df,
                aes(x = water_year,
                    y = value,
                    color = variable)) +
        geom_point(alpha = 0.9, size = 2) +
        scale_color_manual(values = c("#D46F10", "#69B9FA",
                                      "#59A3F8", "#4B8FF7", "#5A7ECB")) +
        labs(x = "Water Year (Oct 1 - Sept 30)",
             title = paste(df$domain[1],
                           df$site_code[1], sep = " ")) +
        theme_bw() +
        theme(legend.position = "none",
              axis.title.y = element_blank()) +
        facet_grid(variable~., scales = "free")

    return(p)

}

lapply(site_sub_list, function(x) ggsave(plot = plotting_covar(x),
                                         filename = paste("figures/site_covariate_plots/",
                                                          x$domain[1],
                                                          x$site_code[1],
                                                          "covar.jpg",
                                                          sep = ""),
                                         width = 20,
                                         height = 18,
                                         units = "cm"))

# End of script.