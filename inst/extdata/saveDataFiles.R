# Converting raw data into package data
library(usethis)
library(magrittr)

#=========================================================
#=========================================================
# POllutants to be included in the model
panel_pollutants <- c("PrimPM25", "NOx", "VOC")
usethis::use_data(panel_pollutants, overwrite = T)

# Panel data
panel_data <- create_panel()
usethis::use_data(panel_data, overwrite = T)








