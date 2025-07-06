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

#### Set up 
# download folder setup

download_folder <- here("data", "raw", "raster")

# Make sure the folder exists
if (!dir.exists(download_folder)) dir.create(download_folder, recursive = TRUE)

# key setup
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
  path     = download_folder
)

###Getting the Income Data...
####################################################### Vector

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
    fill.scale = tm_scale(values = "brewer.greens"),
    fill.legend = tm_legend(title = "Mean Income (€)")
  ) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_title("Average Provincial Income in Spain (2020)")



####################################################### Raster
#Getting temperature data from copernicus

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

# eliminating "Islas Canarias"from the map border to just be mainland

spain_prov_income <- spain_prov_income %>%
  filter(NAME_1 != "Islas Canarias")


# Plotting the temperature data 
tm_shape(spain_prov_income) +
  tm_fill(
    fill = "mean_temp",
    fill.scale = tm_scale(values = "matplotlib.reds"),
    fill.legend = tm_legend(title = "Mean Temperature (°C)")
  ) +
  tm_borders() +
  tm_title("Average Provincial Temperature in Spain in 2020 (Mainland Spain)")


##Scatterplot the relationship between income and temperature

ggplot(spain_prov_income, aes(x = mean_temp, y = mean_income)) +
geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Income vs Temperature in Spain",
       x = "Mean Temperature (°C)",
       y = "Mean Income (€)")

#Dropping NA values
spain_prov_income <- spain_prov_income %>%
  filter(!is.na(mean_income) & !is.na(mean_temp))

#### Simple linear regression model
Regression  <- lm(mean_income ~ mean_temp, data = spain_prov_income)
# Display regression in a table
stargazer(Regression, type = "text", title = "Regression: Income vs Temperature")
# Display regression summary
summary(Regression)


####################################################### Raster
#### We also have the Population density 

# Loading the population density raster
popd_data <- rast(here("data", "raw", "raster", "esp_pd_2020_1km.tif"))
# Ensure popd_data is in EPSG:4326
if (crs(popd_data) != "EPSG:4326") {
  popd_data <- project(popd_data, crs("EPSG:4326"))
}

## Cropping for España
spain <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "Spain")
spain <- st_transform(spain, crs("EPSG:4326"))

# Crop and mask population density raster to Spain
popd_spain <- crop(popd_data, vect(spain))
popd_spain <- mask(popd_spain, vect(spain))


# log the population density raster (doing this to make it look more clear)

popd_spain <- log(popd_spain + 1)  # Adding 1 to avoid log(0)


# plot with x and y limits (mainland España)

plot(popd_spain, main = "Log Population Density in Spain (2020)",
     xlim = c(-10, 5), ylim = c(35, 45))


# Add mean population density to province dataset
spain_prov_income$mean_popd <- exact_extract(
  popd_spain, spain_prov_income, "mean"
)

# Check
head(spain_prov_income[, c("NAME_2", "mean_income", "mean_temp", "mean_popd")])

#### Now each province has mean income, mean temperature, and mean population density.

# Dropping NA values
spain_prov_income <- spain_prov_income %>%
  filter(!is.na(mean_income) & !is.na(mean_temp) & !is.na(mean_popd))

# plot (line) population density and mean income

ggplot(spain_prov_income, aes(x = mean_popd, y = mean_income)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Income vs Population Density in Spain",
       x = "Mean Population Density (log scale)",
       y = "Mean Income (€)") +
  scale_x_log10()  

# top 3 provinces by income
spain_prov_income <- spain_prov_income %>%
  mutate(
    income_label = paste0("€", signif(mean_income / 1000, 3), "k"),
    top3 = rank(-mean_income) <= 3
  )

# top 3 richest 
spain_prov_income %>% filter(top3)

#### Plotting for mean population density and income


tm_shape(spain_prov_income) +
  
  # Main fill: Population Density
  tm_fill(
    fill = "mean_popd",
    fill.scale = tm_scale(values = "brewer.yl_gn_bu"),
    fill.legend = tm_legend(title = "Mean Population Density (log)")
  ) +
  
  # Borders
  tm_borders(col = "black", lwd = 0.5) +
  
  # Missing data
  tm_shape(missing_income_provinces) +
  tm_fill(col = "grey80") +    
  tm_borders(col = "black", lwd = 0.5) +
  
  # Income text (excluding top 3)
  tm_shape(spain_prov_income) +
  tm_text("income_label", size = 0.5, col = "black", shadow = TRUE) +
  
  # Top 3 richest: bold red
  tm_shape(spain_prov_income %>% filter(top3)) +
  tm_text("income_label", size = 0.5, col = "red", fontface = "bold", shadow = TRUE) +
  
  # Manual legend for missing data
  tm_add_legend(
    type = "polygons",
    labels = "N/A",
    col = "grey80",
    size = 0.7
  ) +
  
  # Title and layout
  tm_title("Mean Population Density and Income in Mainland Spain (2020)", size = 0.7) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "white",
    legend.title.size = 0.5,
    legend.text.size = 0.5
  )




# checking the lowest mean income provinces 
spain_prov_income <- spain_prov_income %>%
  mutate(bottom3 = mean_income %in% sort(mean_income, na.last = NA)[1:3])

# Check bottom 3 poorest provinces
spain_prov_income %>% filter(bottom3)

#### Plotting for temperature and income

tm_shape(spain_prov_income) +
  
  # Main fill: Mean Temperature
  tm_fill(
    fill = "mean_temp",
    fill.scale = tm_scale(values = "brewer.yl_or_rd"),
    fill.legend = tm_legend(title = "Mean Temperature (°C)")
  ) +
  
  # Borders
  tm_borders(col = "black", lwd = 0.5) +
  
  # Missing data
  tm_shape(missing_income_provinces) +
  tm_fill(col = "grey80") +    
  tm_borders(col = "black", lwd = 0.5) +
  
  # Income text 
  tm_shape(spain_prov_income) +
  tm_text("income_label", size = 0.5, col = "black", shadow = TRUE) +
  
  # Top 3 richest
  tm_shape(spain_prov_income %>% filter(top3)) +
  tm_text("income_label", size = 0.5, col = "darkgreen", fontface = "bold", shadow = TRUE) +
  
  # Bottom 3 poorest
  tm_shape(spain_prov_income %>% filter(bottom3)) +
  tm_text("income_label", size = 0.5, col = "darkblue", fontface = "bold", shadow = TRUE) +
  
  # Manual legend for missing data
  tm_add_legend(
    type = "polygons",
    labels = "N/A",
    col = "grey80",
    size = 0.7
  ) +
  
  # Title and layout
  tm_title("Mean Temperature and Income in Mainland Spain (2020)", size = 0.7) +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "white",
    legend.title.size = 0.5,
    legend.text.size = 0.5
  )




####Running a linear regression model, with population density as a control variable
Regression_pop <- lm(mean_income ~ mean_temp + mean_popd, data = spain_prov_income)


# Display regression in a table
stargazer(Regression_pop, type = "text", title = "Multiple Regression: Income vs Temperature and Population Density")
# Display the two regression summaries in a table
stargazer(Regression, Regression_pop, type = "text", 
          title = "Comparing Simple OLS and OLS with POP Control",
          column.labels = c("Simple OLS", "Control for Population Density"),
          dep.var.labels = "Mean Income (€)",
          covariate.labels = c("Mean Temperature (°C)", "Mean Population Density (people/km²)"),
          model.numbers = FALSE)

