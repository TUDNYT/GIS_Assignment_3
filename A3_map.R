
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
  tidyr,
  tmap,
  ineAtlas,
  tmap
)
here::i_am("A3_map.R")




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

#####


# Load the temperature raster
temp_tiff <- rast(here("data", "raw", "raster", "era5_temp_20200107.nc"))

# Reproject temp_tiff to EPSG:4326
temp_tiff_4326 <- project(temp_tiff, crs("EPSG:4326"))

# Load the population density raster
popd_data <- rast(here("data", "raw", "raster", "esp_pd_2020_1km.tif"))

# Ensure popd_data is in EPSG:4326
if (crs(popd_data) != "EPSG:4326") {
  popd_data <- project(popd_data, crs("EPSG:4326"))
}

## Cropping for España
spain <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "Spain")
spain <- st_transform(spain, crs("EPSG:4326"))

# Crop the rasters
popd_data_spain <- crop(popd_data, spain)
temp_spain <- crop(temp_tiff_4326, spain)

# Load GADM data for Spain
gadm_spain <- geodata::gadm(country = "ESP", level = 2, path=tempdir())
gadm_spain <- st_as_sf(gadm_spain)  # Convert to sf object
gadm_spain <- st_transform(gadm_spain, crs("EPSG:4326"))

#### Masking the data

# Mask the temperature raster with GADM
spain_temp_mask <- mask(temp_spain, gadm_spain)
# Mask the population density raster with GADM
spain_popd_mask <- mask(popd_data_spain, gadm_spain)

#checking if its the same 
plot(spain_temp_mask, main = "Masked Temperature in Spain")
plot(spain_popd_mask, main = "Masked Population Density in Spain")

#### Resolution

# Comparing the the resolution of the rasters
res(spain_temp_mask)
res(spain_popd_mask)

# Resampling the data to match the resolution of the temperature data
spain_popd_resampled <- resample(spain_popd_mask, spain_temp_mask, method = "bilinear")

# checking if its the same 
res(spain_popd_resampled) # Verify the new resolution
res(spain_temp_mask) # Verify the temperature resolution

#### Double checking provinces in total to then merge with income 


# check the number of provinces in the gadm_spain to see it matches up with our vector file provinces too 
n_provinces <- nrow(gadm_spain)
cat("Number of provinces in Spain:", n_provinces, "\n")

# list the n_provinces
cat("Provinces in Spain:\n")
print(gadm_spain$NAME_1)

#### Income data integration now

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


#### Relationship/ Correlation

#Merging now this data with mean income (spain provinces), with the temperature

head(spain_prov_income) # just to check it has a geo column

# Ensure the CRS of the income data matches the CRS of the spatial data
spain_prov_income <- st_transform(spain_prov_income, crs = st_crs(gadm_spain))

# Extract mean temperature and population density for each province

spain_prov_income$temp_mean <- terra::extract(spain_temp_mask, spain_prov_income, fun = function(x) mean(x, na.rm = TRUE))
spain_prov_income$popd_mean <- terra::extract(spain_popd_resampled, spain_prov_income, fun = function(x) mean(x, na.rm = TRUE))










# Calculate mean temperature and population density
spain_prov_income <- spain_prov_income %>%
  mutate(temp_mean = temp_mean / 10,  # Convert from Kelvin to Celsius
         popd_mean = popd_mean * 1000)  # Convert from people per km² to people per m²








# Calculate correlation
correlation <- cor(spain_prov_income$temp_mean, spain_prov_income$mean_income, use = "complete.obs")
cat("Correlation between mean temperature and mean income:", correlation, "\n")

# Plotting the relationship between mean temperature and mean income
ggplot(spain_prov_income, aes(x = temp_mean, y = mean_income)) +
  geom_point(aes(color = NAME_2), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship between Mean Temperature and Mean Income in Spanish Provinces",
    x = "Mean Temperature (°C)",
    y = "Mean Income (€)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#### Mapping 

# Temperature Map
tm_shape(spain_prov_income) %>%
  tm_fill("temp_mean",
          fill.scale = tm_scale_intervals(),
          col = "RdBu",
          fill.legend = tm_legend("Mean Temperature (°C)")) +
  tm_borders() +
  tm_title("Mean Temperature by Province")

# Income Map
tm_shape(spain_prov_income) %>%
  tm_fill("mean_income", palette = "YlGnBu", title = "Mean Income (€)") +
  tm_borders() +
  tm_layout(title = "Average Provincial Income in Spain (2020)")

# Combined Map (Temperature and Income)
tm_shape(spain_prov_income) %>%
  tm_fill("mean_income", palette = "YlGnBu", title = "Mean Income (€)") +
  tm_fill("temp_mean", palette = "RdBu", alpha = 0.5, title = "Mean Temperature (°C)") + # Add temperature as a semi-transparent layer
  tm_borders() +
  tm_layout(title = "Temperature and Income by Province")
