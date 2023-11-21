library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

setwd(here::here())

datadir <- paste0(getwd(),"/extdata") 


reg_iso<-read.csv(paste0(datadir,"/iso_region.csv"))

resid_em_kt<-dplyr::bind_rows(
              read.csv(paste0(datadir,"/emissions/BC_total_CEDS_emissions.csv"), skip = 6) %>% mutate(ghg = "BC"),
              read.csv(paste0(datadir,"/emissions/CO_total_CEDS_emissions.csv"), skip = 7) %>% mutate(ghg = "CO"),
              read.csv(paste0(datadir,"/emissions/CH4_total_CEDS_emissions.csv"), skip = 7) %>% mutate(ghg = "CH4"),
              read.csv(paste0(datadir,"/emissions/N2O_total_CEDS_emissions.csv"), skip = 6) %>% mutate(ghg = "N2O"),
              read.csv(paste0(datadir,"/emissions/NH3_total_CEDS_emissions.csv"), skip = 6) %>% mutate(ghg = "NH3"),
              read.csv(paste0(datadir,"/emissions/NMVOC_total_CEDS_emissions.csv"), skip = 6) %>% mutate(ghg = "NMVOC"),
              read.csv(paste0(datadir,"/emissions/NOx_total_CEDS_emissions.csv"), skip = 6) %>% mutate(ghg = "NOx"),
              read.csv(paste0(datadir,"/emissions/OC_total_CEDS_emissions.csv"), skip = 7) %>% mutate(ghg = "OC"),
              read.csv(paste0(datadir,"/emissions/SO2_total_CEDS_emissions.csv"), skip = 7) %>% mutate(ghg = "SO2")) %>%
      tidyr::gather(year, value, -iso, -sector, -ghg ,-fuel, -units) %>%
      dplyr::mutate(year = gsub("X","",year)) %>%
      dplyr::filter(grepl("Residential",sector)) %>%
      dplyr::group_by(iso, sector, ghg, year , units) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      tidyr::spread(ghg, value) %>%
      dplyr::mutate(year = as.numeric(year))


mort<-tibble::as_tibble(dplyr::bind_rows(read.csv(paste0(datadir, "/mort/IHME-GBD_2019_DATA-15769af1-1.csv")),
                       read.csv(paste0(datadir, "/mort/IHME-GBD_2019_DATA-15769af1-2.csv")))) %>%
  dplyr::select(country_name = location_name, rei_name, measure_name, cause_name, year, sex_name, val) %>%
  dplyr::filter(sex_name == "Both") %>%
  tidyr::spread(measure_name, val) %>%
  gcamdata::left_join_error_no_match(reg_iso, by = "country_name") 

#-------------
# Check the country names that need to be manually adjusted
#names_ok<-mort %>%  dplyr::filter(complete.cases(.)) %>% select(country_name) %>% distinct()
#names_all<-mort %>% select(country_name) %>% distinct()
#names_missing<- anti_join(names_all,names_ok, by = "country_name")
#-------------


data<-mort %>%
  dplyr::left_join(resid_em_kt %>% select(-sector, -units), by = c("iso","year"))

#-------------
# Check missing data
#data_complete<- data %>% filter(!grepl("Sense", cause_name)) %>% filter(complete.cases(.))
#data_missing<- anti_join(data %>% filter(!grepl("Sense", cause_name)), data_complete)
#unique(data_missing$country_name)
#-------------

# Load all the socioeconomic data to be included
pop<- read.csv(paste0(datadir, "/socioeconomic/population.csv")) %>%
  dplyr::filter(year %in% unique(data$year)) %>%
  dplyr::mutate(iso = tolower(iso)) %>% 
  select(-country_name)

gdp<- read.csv(paste0(datadir, "/socioeconomic/gdp.csv")) %>%
  select(country_name, iso, year, gdp_ppp_dol2011) %>%
  dplyr::filter(year %in% unique(data$year)) %>%
  dplyr::mutate(iso = tolower(iso)) %>% 
  select(-country_name)

urbrur<-read.csv(paste0(datadir, "/socioeconomic/urb_rur_shares.csv"))%>%
  dplyr::filter(year %in% unique(data$year)) %>%
  dplyr::mutate(iso = tolower(iso)) %>% 
  select(-country_name) %>%
  filter(iso != "")

elec_acces<-read.csv(paste0(datadir, "/socioeconomic/elec_access.csv")) %>%
  dplyr::filter(year %in% unique(data$year)) %>%
  dplyr::mutate(iso = tolower(iso)) %>% 
  select(-country_name)  %>%
  filter(iso != "")

cc_acces<-read.csv(paste0(datadir, "/socioeconomic/clean_cook_access.csv")) %>%
  dplyr::filter(year %in% unique(data$year)) %>%
  dplyr::mutate(iso = tolower(iso)) %>% 
  select(-country_name) %>%
  filter(iso != "")


# Create the dataset with all the socioeconomic data
socio<-tibble::as_tibble(pop) %>%
  #gcamdata::left_join_error_no_match(gdp) %>%
  dplyr::left_join(gdp, by = c("iso", "year")) %>%
  dplyr::left_join(urbrur, by = c("iso", "year")) %>%
  dplyr::left_join(elec_acces, by = c("iso", "year")) %>%
  dplyr::left_join(cc_acces, by = c("iso", "year")) 


# Combine all the final data to be analyzed:
data_fin<-data %>%
  dplyr::left_join(socio, by = c("iso","year")) %>%
  mutate(gdppc_ppp_dol2011 = gdp_ppp_dol2011 / pop) %>%
  # Make some adjustments for economeric analysis
  mutate(PrimPM25 = BC + OC)

#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Start of the econometric analysis:
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# First create a list: split by disease for more detailed outputs:
data_fin_list <- split(data_fin, data_fin$cause_name)

# For the moment, use one disease -> then extend to the full list
data_fin_card <- data_fin %>%
  dplyr::filter(cause_name == "Cardiovascular diseases")

#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Panel linear models (https://www.youtube.com/watch?v=1pST2lUx6QM)
# install.packages("plm")
library(plm)


# First select de endogenous variable (Y) and covariates (X)
colnames(data_fin_card)

attach(data_fin_card)

# We select deaths as endogenous variable
# OPTIONS: "DALYs (Disability-Adjusted Life Years)"; "Deaths"; "YLLs (Years of Life Lost)"

Y <- cbind(Deaths)
X <- cbind(PrimPM25, CH4, CO, N2O, NH3, NMVOC, NOx, SO2,
           rural, elec_access_perc, cc_access_perc, gdppc_ppp_dol2011)

# Set data as panel data:
pdata <- pdata.frame(data_fin_card, index = c("iso", "year"))

# Descriptive statistics:
summary(Y)
summary(X)

# Pooled OLS estimator
pooling <- plm(Y ~ X, data = pdata, model = "pooling")
summary(pooling)

# Between estimator
between <- plm(Y ~ X, data = pdata, model = "between")
summary(between)

# First differences estimator
firstdiff <- plm(Y ~ X, data = pdata, model = "fd")
summary(firstdiff)

# Fixed effects estimator
fixed <- plm(Y ~ X, data = pdata, model = "within")
summary(fixed)

# Random effects estimator
random <- plm(Y ~ X, data = pdata, model = "random")
summary(random)

# Do tests to select the model:

# LM test for random versus OLS (small p-values -> better the random effects)
plmtest(pooling)

# F test for random versus OLS (small p-values -> better the fixed effects)
pFtest(fixed, pooling)

# Hausmann test for fixed vs random models:
phtest(random, fixed)

# If p-value is small and one model is inconsistent, need to go to fixed effects!













