# Discharge PCA
# August 26, 2024
# Heili Lowman

#### READ ME ####

# The following script will create a PCA figure to examine
# the discharge summary metrics across sites.

# Note, this script uses a newer version of the data (v2) sent by
# Mike on 8/19/24, not the current package version of the dataset.

#### Setup ####
source(here('src', 'setup.R'))

# Load data with discharge metrics.
q_metrics <- readRDS("data_working/discharge_8metrics.rds")

# And load trends data.
hydro_trend_data <- read_csv("data_working/hydro_climate_trends.csv")

#### Tidy ####

# We only want to keep the indices that will be read into our PCA.
q_dat <- q_metrics %>%
    column_to_rownames(var = "site_code") %>%
    select(m1_meanq, m2_cvq, m3_skewq, m4_kurtq,
           m5_ar1q, m6_ampq, m7_phiq, rbiq)

# Also, need to denote trends in mean annual precip (since this was
# the most divergent in previous analysis).
ppt_trend_data <- hydro_trend_data %>%
    filter(var == "precip_mean_ann") %>%
    mutate(sig = case_when(p < 0.05 ~ "YES",
                           TRUE ~ "NO")) %>%
    mutate(direction = factor(case_when(sig == "YES" & trend > 0 ~ "+",
                                        sig == "YES" & trend < 0 ~ "-",
                                        TRUE ~ "no trend"),
                              levels = c("+", "-", "no trend")))

#### PCA ####

# Fit principal component analysis for all sites with discharge metrics.
qPCA <- prcomp(q_dat,
               center = TRUE,
               scale = TRUE)

# Examine output.
summary(qPCA)

# Pull out PCA score values.
qPCA_scores <- qPCA$x %>%
    as.data.frame() %>%
    rownames_to_column(var = "site_code")

# Pull out PCA eigenvectors.
qPCA_vectors <- qPCA$rotation %>%
    as.data.frame()

#### Plot ####

(fig_PCApts <- ggplot() +
    # add site-level points
    geom_point(data = qPCA_scores %>%
                   left_join(ppt_trend_data) %>%
                   left_join(ms_site_data) %>%
                   filter(sig == "YES"),
               aes(x = PC1, y = PC2, color = direction),
               alpha = 0.75, size = 3) +
    scale_color_manual(values = c("#3793EC", "#F28023", "#DCC8BA")) +
    theme_bw() +
    labs(x = "PC1 (47.09%)",
         y = "PC2 (20.86%)",
         color = "Trend in MAP"))

(fig_PCAvcts <- ggplot() +
    # add eigenvectors
    geom_segment(data = qPCA_vectors, aes(x = 0, y = 0,
                                          xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    # add eigenvector labels
    geom_text(data = qPCA_vectors, aes(x = PC1, y = PC2,
                                       label = rownames(qPCA_vectors))) +
    theme_bw() +
    labs(x = "PC1 (47.09%)",
         y = "PC2 (20.86%)",
         title = "PCA of Macroshed Q Metrics"))

(fig_PCA <- fig_PCAvcts + fig_PCApts)

ggsave(plot = fig_PCA,
       filename = "figures/hydro_pca_082624.jpg",
       width = 25,
       height = 10,
       units = "cm")

# End of script.
