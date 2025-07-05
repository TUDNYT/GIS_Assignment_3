
#### Setup 

library(pacman)

pacman::p_load(
  httr,
  jsonlite,exactextractr,
  ggplot2,
  keyring,
  ecmwfr,
  dplyr,
  reticulate,
  geodata,
  sf,
  rvest,
  raster,
  rnaturalearth,
  rnaturalearthdata,
  grid,
  readr,
  ggspatial,
  gridExtra,
  ggrepel,
  tidygeocoder,
  osrm ,
  here,
  terra,
  tidyr
)
here::i_am("A3.R")




key_set_with_value(
  service  = "cds",
  username = "nykol_yove.armacanqui_martinez_de_tudor@mailbox.tu-dresden.de",
  password = "3aa6e68c-6a56-4adb-a9c4-2e6220ac9806"
)

# now pull it back out:

cds_api_key <- key_get(
  service  = "cds",
  username = "nykol_yove.armacanqui_martinez_de_tudor@mailbox.tu-dresden.de"
)


# Export to environment variables for cdsapi, reticulate, or ecmwfr

Sys.setenv(
  CDSAPI_URL = "https://cds.climate.copernicus.eu/api",
  CDSAPI_KEY = cds_api_key
)


wf_set_key(
  key  = cds_api_key
)

# if you ever want to remember your API key 


wf_get_key()

request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type       = "reanalysis",
  variable           = "2m_temperature",
  year               = "2020",
  month              = "07",
  day                = "01",
  time               = "15:00",
  data_format        = "netcdf",
  download_format    = "unarchived",
  target             = "era5_temp_20200107.nc" 
)

era5 <- wf_request(
  request  = request,
  transfer = TRUE,
  path     = getwd()
)

######
temp_tiff <- rast(here("data", "raw", "raster", "era5_temp_20200107.nc"))


####Starts here to get the income data################################################

####Read the income_municipalities csv file
income_municipalities <- read_csv(here("data", "raw", "income_mun.csv"))

##Libraries
# Install if not already installed
install.packages("ineAtlas")



library(ineAtlas)
library(sf)
# Function to get municipality georeferenced
head(income_municipalities)

library(geodata)

# Download GADM level 2: Spain Provinces
spain_prov <- geodata::gadm(country = "ESP", level = 2, path = tempdir())

# Quick check
plot(spain_prov["NAME_2"], main = "Spain Provinces")

library(dplyr)

# Filter to a single year (2020)
income_2020 <- income_municipalities %>%
  filter(year == 2020)

# Aggregate mean income by province
prov_income <- income_2020 %>%
  group_by(prov_name) %>%
  summarise(mean_income = mean(net_income_pc, na.rm = TRUE))

# Check
head(prov_income)

unique(spain_prov$NAME_2)
unique(prov_income$prov_name)
library(sf)

# Convert SpatVector to sf object
spain_prov_sf <- sf::st_as_sf(spain_prov)

## Join the income data with the spatial data..

spain_prov_income <- spain_prov_sf %>%
  left_join(prov_income, by = c("NAME_2" = "prov_name"))
##Map
library(tmap)

tm_shape(spain_prov_income) +
  tm_fill("mean_income", palette = "YlGnBu", title = "Mean Income (€)") +
  tm_borders() +
  tm_layout(title = "Average Provincial Income in Spain (2020)")

