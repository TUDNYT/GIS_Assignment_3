
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

#####

temp_tiff <- rast(here("data", "raw", "raster", "era5_temp_20200107.nc"))

#plot the tiff file
plot(temp_tiff)

#Checking CRS of the tiff file and see whether it needs flipping along the y axis
crs(temp_tiff)

plot(temp_tiff, main = "Temperature")

popd_data <- here("data", "raw", "raster", "esp_pd_2020_1km.tif") %>%
  rast()
#Checking the CRS of the tiff file
crs(popd_data)
crs(popd_data) <- "EPSG:4326"
plot(popd_data, main = "Population Density (2020)")

##Cropping for España
spain <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "Spain")
spain <- st_transform(spain, crs(popd_data))
popd_data_spain <- crop(popd_data, spain)
plot(popd_data_spain, main = "Population Density in Spain (2020)")

plot(popd_data)
plot(temp_tiff)

##### Adding 

gadm_spain <- geodata::gadm(country = "ESP", level = 1, path=tempdir())

glimpse(gadm_spain)
plot(gadm_spain["NAME_1"], main = "Spain: Admin Level 1")

#Box around india map
box <- st_bbox(gadm_spain)

## now we can use the crop function 

spain_temp <- crop(temp_tiff,gadm_spain)  

spain_popd <- crop(popd_data,gadm_spain)  
plot(spain_temp)

plot(spain_popd)
