#' Function that downloads 8-day composite MODIS LST data.
#' The monthly average is generated depending on the chosen date.
#'
#' The output is a "GTiff" file format for the selected Country or Region.
#'
#'
#'
#' @param sen a character vector of MODIS Sensor name "Terra, Aqua".
#' @param usr a character vector of USERNAME of Earthdata Login.
#' @param pass a character vector of PASSWORD of Earthdata Login.
#' @param bd a character vector, format YYYY.MM.DD, of Start date
#' @param ed a character vector, format YYYY.MM.DD, of End date
#' @param mnth a character vector of Month, "Jan, Feb, March, ..."
#' @param proj a numeric vector, ESPG Projection number
#' @param cntr a character vector, NAME of the country
#' @param sta a LOGIC vector, Load Country or Region Boundaries
#' @param reg a character vector, name of the Region of the Country
#'
#' @import terra
#' @import MODIStsp
#' @import sf
#' @import rnaturalearth
#' @import sp
#' @import rnaturalearthdata
#' @import rnaturalearthhires
#' @importFrom methods as
#'
#' @examples
#'
#' The example is for the Region of Puno, for the year 2010 in the month of March.
#'
#' mod_lst (sen = "Aqua", usr = "own_user", pass = "own_password",
#'           bd = "2010.03.01", ed = "2010.03.31", mnth = "March",
#'           proj = 4326, cntr = "peru",sta = TRUE, reg = "Puno")
#'
#'
#' @export
#'

mod_lst  <- function(sen, usr, pass, bd, ed, mnth, proj, cntr, sta, reg){

  #------ Create directory MODIS and tif
  datadir <- file.path(paste0(getwd(), "/MODIS"))
  dir.create(datadir, showWarnings=FALSE)

  datatif <- file.path(paste0(getwd(), "/tif"))
  dir.create(datatif, showWarnings=FALSE)


  ##----- Load Country Boundaries and select states or not
  if (sta == TRUE) {
    st <- ne_states(country = cntr, returnclass = "sf")
    #reg <- readline(prompt = c(cat(paste("Select region:  ", st$name_en), sep='\n'), "Write region: ") )
    position <- which(st$name_es == reg)
    st <- st[position, ]
    numeric_coordinates <- st_bbox(st)
  }else{
    st <- ne_countries(country = cntr, returnclass = "sf")
    numeric_coordinates <- st_bbox(st)
  }



  #------ Download MODIS LST 8-Days Composite in GTiff format
  mf <- MODIStsp::MODIStsp(gui = FALSE, out_folder = datadir, selprod = "Surf_Temp_8Days_1Km (M*D11A2)",
                           bandsel = "LST_Day_1km", sensor = sen, user = usr,password = pass, start_date = bd, end_date = ed,
                           out_format = "GTiff", delete_hdf = TRUE, spatmeth = "bbox",
                           bbox = numeric_coordinates, output_proj = proj)


  #------ Processing for Monthly calculation of LST composite
  # Select all TIF files downloaded and save as a List
  tif_files <- list.files(path = paste0(datadir,"/Surf_Temp_8Days_1Km_v61/LST_Day_1km/"),
                          pattern = "tif$", full.names = TRUE)


  # Stack all the files
  temperature_stack <- stack(tif_files)

  # Get the Mean of all stack and transform the temperature to Celsius
  average_temperature <- mean(temperature_stack)*0.02 - 273.15

  #----- Crop to the boundaries
  # Convert sf object to SpatialPolygonsDataFrame
  spatial_polygons <- methods::as(st, "Spatial")

  # Ensure the coordinate reference systems match
  crs(spatial_polygons) <- crs(average_temperature)

  # Crop the raster with the SpatialPolygonsDataFrame
  cropped_raster <- mask(average_temperature, mask=spatial_polygons, inverse=FALSE)

  # Create a new TIF file of the Average temperature
  writeRaster(cropped_raster, filename = paste0(datatif,"/LST_mean_",mnth,".tif"), format = "GTiff", overwrite = TRUE)

}



# Install packages if not already installed

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("devtools", quietly = TRUE))  install.packages("devtools")
if (!requireNamespace("MODIStsp", quietly = TRUE))  devtools::install_github("ropensci/MODIStsp")
if (!requireNamespace("rnaturalearth", quietly = TRUE))  devtools::install_github("ropensci/rnaturalearth")
if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) devtools::install_github("ropensci/rnaturalearthdata")
if (!requireNamespace("rnaturalearthhires", quietly = TRUE))  devtools::install_github("ropensci/rnaturalearthhires")


# Load Libraries
library(terra)
library(MODIStsp)
library(sf)
library(rnaturalearth)
library(sp)
library(rnaturalearthdata)
library(rnaturalearthhires)














