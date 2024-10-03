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
    dplyr::filter(., site_code == 'GSLOOK') %>%
    left_join(., enso_ann, by = 'water_year') %>%
    select(water_year, precip_mean_ann, NPGO) %>%
    na.omit()


# Convert the 'year' column to a time series object
ts_precip <- ts(enso_metrics$precip_mean_ann, start = min(enso_metrics$water_year), frequency = 1)

# Plot the time series to visualize it
plot(ts_precip, main = "Annual Precipitation Over Time", ylab = "Annual Precipitation", xlab = "Year")

# If you want to consider the ENSO index as a covariate:
# Fit the Auto ARIMA model with the ENSO index as an external regressor (xreg)
model_test <- auto.arima(ts_precip, xreg = enso_metrics$water_year)
# model_test2 <- auto.arima(ts_precip)
summary(model_test)
# summary(model_test2)

# test3 <- summary(lm(data = enso_metrics, precip_mean_ann ~ water_year))
# test3

test4 <- sens.slope(ts_precip)
test4

# calc p values
# from https://www.geeksforgeeks.org/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r/
# Extract coefficients
coefs <- coef(model_test)
print(coefs)

fit<-model_test

# Calculate the t-values
t_values <- coefs / 2e-04

# Calculate the p-values
p_values <- 2 * (1 - pnorm(abs(t_values[[1]])))

plot(ts_precip)

fitted_values <- fitted(model_npgo)

checkresiduals(model_npgo)

coef(model_npgo)

# Now we need to convert this frequency to actual time periods by taking the inverse of the frequency.
TimePeriod<-1/top2[2,1]
TimePeriod


# now make ts with that period
ts_precip_cyc <- ts(enso_metrics$precip_mean_ann, start = min(enso_metrics$water_year), end = max(enso_metrics$water_year), frequency =  10)

decomp <- stl(ts_precip_cyc, s.window="periodic")
autoplot(decomp)
