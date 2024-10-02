# handle setup
library(here)
source(here('src', 'setup.R'))

library(forecast)
library(rsoi)
library(timeSeries)
library(timeDate)

# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS')) %>%
    distinct()


# climate ####
enso <- download_enso()

enso_ann <- enso %>%
    mutate(water_year = water_year(Date, origin = 'usgs'),
           water_year = as.integer(as.character(water_year))) %>%
    select(-phase, -Month, -Date, - Year) %>%
    pivot_longer(cols = -c('water_year'), names_to = 'var', values_to = 'val') %>%
    group_by(water_year, var) %>%
    summarize(val = mean(val)) %>%
    pivot_wider(names_from = 'var', values_from = 'val')


# ####

enso_metrics <- metrics %>%
    dplyr::filter(., site_code == 'w6') %>%
    left_join(., enso_ann, by = 'water_year') %>%
    select(water_year, precip_mean_ann, NPGO) %>%
    na.omit()


# Convert the 'year' column to a time series object
ts_precip <- ts(enso_metrics$precip_mean_ann, start = min(enso_metrics$water_year), frequency = 1)

# Plot the time series to visualize it
plot(ts_precip, main = "Annual Precipitation Over Time", ylab = "Annual Precipitation", xlab = "Year")

components.ts = decompose(ts_precip)


# If you want to consider the ENSO index as a covariate:
# Fit the Auto ARIMA model with the ENSO index as an external regressor (xreg)
model_npgo <- auto.arima(ts_precip, xreg = enso_metrics$NPGO)

summary(model_npgo)

fitted_values <- fitted(model_npgo)

checkresiduals(model_npgo)

#install.packages("TSA") #if needed
library("TSA")
periodogram(ts_precip)


# Create a plot for the actual vs fitted values
ggplot(fitted_values, aes(x = water_year)) +
    geom_line(aes(y = ts_precip), color = 'Actual') +
    geom_line(aes(y = fitted_values), color = 'Fitted') +
    labs(title = 'Actual vs Fitted Annual Precipitation',
         x = 'Year',
         y = 'Annual Precipitation') +
    scale_color_manual(values = c('Actual' = 'blue', 'Fitted' = 'red')) +
    theme_minimal()
