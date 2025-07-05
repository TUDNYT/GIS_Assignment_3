#### Setup 

library(pacman)

pacman::p_load(
  httr,
  jsonlite,exactextractr,stargazer,
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
  tidyr,
  tmap,
  ineAtlas,
  tmap
)
here::i_am("A3.R")  


###Getting the Income Data...
#######################################################

#reading the income_municipalities csv file
income_municipalities <- read_csv(here("data", "raw", "vector", "income_mun.csv"))

spain_prov <- geodata::gadm(country = "ESP", level = 2, path = tempdir())

#checking 
plot(spain_prov["NAME_2"], main = "Spain Provinces")

#filter to year of interest 

income_2020 <- income_municipalities %>%
  filter(year == 2020)

# Aggregate mean income by province
prov_income <- income_2020 %>%
  group_by(prov_name) %>%
  summarise(mean_income = mean(net_income_pc, na.rm = TRUE))

# checking 

head(prov_income)

unique(spain_prov$NAME_2)
unique(prov_income$prov_name)

# Convert SpatVector to sf object
spain_prov_sf <- sf::st_as_sf(spain_prov)

###Getting Spain Boundaries from GADM
#######################################################
# Load GADM data for Spain
gadm_spain <- geodata::gadm(country = "ESP", level = 2, path=tempdir())
gadm_spain <- st_as_sf(gadm_spain)  # Convert to sf object
gadm_spain <- st_transform(gadm_spain, crs("EPSG:4326"))


# Joining the income data with the spatial data GADM

spain_prov_income <- gadm_spain %>%
  left_join(prov_income, by = c("NAME_2" = "prov_name"))

#### Mapping

tm_shape(spain_prov_income) +
  tm_fill(
    fill = "mean_income",
    fill.scale = tm_scale(values = "brewer.yl_gn_bu"),
    fill.legend = tm_legend(title = "Mean Income (€)")
  ) +
  tm_borders() +
  tm_title("Average Provincial Income in Spain (2020)")


###############################################
#Getting temperature data from copernicus##################



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

# download folder 

download_folder <- here("data", "raw", "raster")

# Make sure the folder exists
if (!dir.exists(download_folder)) dir.create(download_folder, recursive = TRUE)

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
  path     = download_folder
)

# Load the temperature raster
temp_tiff <- rast(here("data", "raw", "raster", "era5_temp_20200107.nc"))

# Reproject temp_tiff to EPSG:4326
temp_tiff_4326 <- project(temp_tiff, crs("EPSG:4326"))

# Clip temperature raster to Spain's extent
temp_spain <- crop(temp_tiff_4326, spain_prov_sf)
temp_spain <- mask(temp_spain, spain_prov_sf)

# Plot to check
plot(temp_spain, main = "Temperature in Spain (July 1, 2020 at 15:00)")

# Extract mean temperature per province
spain_prov_income$mean_temp <- exact_extract(temp_spain, spain_prov_income, "mean")

# Check result
head(spain_prov_income[, c("NAME_2", "mean_income", "mean_temp")])

# Plotting the temperature data 
tm_shape(spain_prov_income) +
  tm_fill(
    fill = "mean_temp",
    fill.scale = tm_scale(values = "matplotlib.reds"),
    fill.legend = tm_legend(title = "Mean Temperature (°C)")
  ) +
  tm_borders() +
  tm_title("Average Provincial Temperature in Spain ")

##Scatterplot the relationship between income and temperature

ggplot(spain_prov_income, aes(x = mean_temp, y = mean_income)) +
geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Income vs Temperature in Spain",
       x = "Mean Temperature (°C)",
       y = "Mean Income (€)")
###Simple linear regression model
Regression  <- lm(mean_income ~ mean_temp, data = spain_prov_income)
# Display regression in a table
stargazer(Regression, type = "text", title = "Regression: Income vs Temperature")
# Display regression summary
summary(Regression)
