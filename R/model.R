library(dplyr)
library(plm)
library(ggplot2)

datadir <- paste0(getwd(),"/inst/extdata")
# ----------------------------------------------
# Adjust the data
data <- a %>%
  select(iso, country_name, year, pop, starts_with("log")) %>%
  select(-log_AAP) %>%
  filter(complete.cases(.)) %>%
  # adjust Taiwan Flsp
  mutate(log_flsp = if_else(year == 2019 & iso == "twn", 1.729749, log_flsp))

# ----------------------------------------------
# ----------------------------------------------
# FIT THE MODEL
# ----------------------------------------------
# 1- Test the fixed effects model
fixed <- plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k + log_NOx_per_100k +
               log_gdppc_ppp_dol2011 +
               log_flsp +
                log_HDD_value +
               log_CDD_value,
             data = data,
             index=c("country_name", "year"), model="within")
summary(fixed)
# HDD is non-significative and CDD has a strange effect
fixed_noDD <- plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k + log_NOx_per_100k +
                   log_gdppc_ppp_dol2011 +
                   log_flsp,
                 data = data,
                 index=c("country_name", "year"), model="within")
summary(fixed_noDD)

# AIC cannot be directly used with panel data
# R2 does is not largely reduced, so we select the no_DD model
fixed_fin <- plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k + log_NOx_per_100k +
               log_gdppc_ppp_dol2011 +
               log_flsp,
             data = data,
             index=c("country_name", "year"), model="within")
summary(fixed_fin)

# ----------------------------------------------
# 2- Use the Hausman test to check fixed effects are more accurate than random
# Fit a RE model
random <- plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k + log_NOx_per_100k +
                   log_gdppc_ppp_dol2011 +
                   log_flsp,
                 data = data,
                 index=c("country_name", "year"), model = "random")
summary(random)

phtest(fixed_fin,random) #Hausman test

# Given that p-value < 0.01 -> FIXED effects!

# ----------------------------------------------
# 3- Check multicolineality
# VIF does not work for FE or RE models -> need pooled model and check
pooled <- plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k + log_NOx_per_100k +
               log_gdppc_ppp_dol2011 +
               log_flsp,
             data = data,
             index=c("country_name", "year"), model="pooling")
summary(pooled)
car::vif(pooled)
#create vector of VIF values
vif_values <- car::vif(pooled)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

# VIF values are around 2 -> no Multicolinearity.

# ----------------------------------------------
# 4- Test for time-fixed effects
# fixed_fin_time <- plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k + log_NOx_per_100k +
#                         log_gdppc_ppp_dol2011 +
#                         log_flsp +
#                         factor(year),
#                       data = data,
#                       index=c("country_name", "year"), model="within")
# summary(fixed_fin_time)
#
# pFtest(fixed_fin, fixed_fin_time)

# ----------------------------------------------
# Ver el resumen del modelo
fixed_fin <- plm(log_Deaths_per_100k ~ log_PrimPM25_per_100k + log_NOx_per_100k +
                   log_gdppc_ppp_dol2011 +
                   log_flsp,
                 data = data,
                 index=c("country_name", "year"), model="within")
summary(fixed_fin)

# ----------------------------------------------
# ----------------------------------------------
# PREDICTIONS
# ----------------------------------------------
#data.panel <- plm::pdata.frame(data, index = c("country_name", "year"))

data$pred_log_Deaths_per_100k <- predict(fixed_fin, data)

# Add GCAM_region
iso_GCAM_regID <- read.csv(paste0(datadir, "/iso_GCAM_regID.csv")) %>%
  select(iso, GCAM_region)

data_check <- data %>%
  mutate(Deaths = exp(log_Deaths_per_100k) * pop / 100000) %>%
  mutate(pred_Deaths = exp(pred_log_Deaths_per_100k) * pop / 100000) %>%
  select(iso, country_name, year, pop, Deaths, pred_Deaths) %>%
  left_join(iso_GCAM_regID, by = join_by(iso)) %>%
  rename(country = country_name) %>%
  mutate(Deaths_per_100K = Deaths / pop * 1E5,
         pred_Deaths_per_100K = pred_Deaths / pop * 1E5)


ggplot(data_check, aes(x = Deaths_per_100K, y = pred_Deaths_per_100K, colour = GCAM_region)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0,
              slope = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  xlim(0,1000) +
  ylim(0,1000)

# Adjustment: Calculate a bias adder as the difference between predicted and observed deaths in final year
data_adder <- data_check %>%
  filter(year == max(year)) %>%
  mutate(bias.adder = Deaths_per_100K - pred_Deaths_per_100K) %>%
  select(iso, country, bias.adder)

data_check_fin <- data_check %>%
  gcamdata::left_join_error_no_match(data_adder, by = join_by(iso, country)) %>%
  mutate(pred_Deaths_per_100K_adj = pred_Deaths_per_100K + bias.adder,
         pred_Deaths_adj = pred_Deaths_per_100K_adj * pop / 100000,
         test = Deaths - pred_Deaths_adj) %>%
  select(iso, country, year, Deaths, pred_Deaths = pred_Deaths_adj, GCAM_region) %>%
  group_by(GCAM_region, year) %>%
  summarise(Deaths = sum(Deaths),
            pred_Deaths = sum(pred_Deaths)) %>%
  ungroup() %>%
  mutate(Deaths = round(Deaths, 0),
         pred_Deaths = round(pred_Deaths, 0))

selected_regions <- c("Africa_Eastern", "China", "Europe_Eastern", "India",
                      "Indonesia", "South Asia", "EU-15", "USA")

ggplot(data_check_fin, aes(x = Deaths, y = pred_Deaths, colour = GCAM_region)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0,
              slope = 1) +
  theme_bw() +
  theme(legend.position = "none")







