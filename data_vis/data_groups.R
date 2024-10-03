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


