# handle setup
library(here)
source(here('src', 'setup.R'))

# load in q data and remove duplicates
q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F
) %>%
    distinct(., site_code, datetime, .keep_all = TRUE) %>%
    filter(ms_interp == 0)

#

ggplot(q_data, aes(x = datetime, y = site_code))+
    geom_point()
