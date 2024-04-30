#'
#' This function generates data interpolation using the IDW method. The monthly
#' data acquired was obtained from meteorological stations with maximum and
#' minimum temperature information for the year 2010 in March, for the whole
#' region of Puno.
#'
#'
#'
#' @param dat the path of the CSV file to read.
#' @param sta a LOGIC character if you need Country or Regional Boundary.
#' @param cntr a character vector of country name
#' @param coun_cd a character vector of ISO CODE of the country
#' @param alt a LOGIC character, if you need the Altitude or not
#' @param reg a character vector, name of the Region of the Country
#' @param temp a character vector, select "tx" or "tn" for max or min temperature
#'
#'
#' @import geodata
#' @import tidyverse
#' @import gstat
#' @import sf
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @import rnaturalearthhires
#' @import raster
#' @import readr
#' @importFrom dplyr mutate
#' @importFrom methods as
#' @import ggplot2
#' @import RColorBrewer
#'
#'
#'
#'
#' @export

idw_inter <- function (dat, sta, cntr, coun_cd, alt, reg,temp){

  if (sta == TRUE) {
    st <- ne_states(country = cntr, returnclass = "sv")
    position <- which(st$name_es == reg)
    st <- st[position, ]
    numeric_coordinates <- st_bbox(st)
  }else{
    st <- ne_countries(country = cntr, returnclass = "sv")
    numeric_coordinates <- st_bbox(st)
  }

  #------ Create directory CSV files
  datadir <- file.path(paste0(getwd(), "/IDW"))
  dir.create(datadir, showWarnings=FALSE)


  # Load data ---------------------------------------------------------------
  tble <- dat # "temp.csv"

  # If dont have altitude from stations get it from srtm ---------------------------------------------
  if(alt == FALSE){

    srtm <- geodata::elevation_30s(country = coun_cd, level = 1, path = './srtm')
    tble <- dplyr::mutate(tble, alt = terra::extract(srtm, tble[,c('Lon', 'Lat')])[,2])
  }

  # Project -----------------------------------------------------------------
  # Define the geographic coordinate reference system (CRS)
  geog <- '+proj=longlat +datum=WGS84 +no_defs'

  # Ensure the input data table has the necessary columns
  if (!all(c("Lon", "Lat", "Alt") %in% names(tble))) {
    stop("Input table must include 'Lon', 'Lat', and 'Alt' columns.")
  }

  # Convert data frame to a spatial vector with longitude and latitude
  pnts <- vect(tble, c("Lon", "Lat"), crs = geog)


  # Function to calculate UTM zone based on longitude
  get_utm_zone <- function(longitude) {
    zone <- floor((longitude + 180) / 6) + 1
    return(zone)
  }

  # Calculate the median longitude to determine the most representative UTM zone
  median_lon <- median(tble$Lon)
  utm_zone <- get_utm_zone(median_lon)

  # Define the UTM projection string dynamically
  proj <- sprintf('+proj=utm +zone=%d +datum=WGS84 +units=m +no_defs', utm_zone)

  # Project the points to the dynamically determined UTM
  pnts <- project(pnts, proj)

  # Filter out points with NA in the 'Altura' column
  pnts <- pnts[!is.na(pnts$Alt), ]

  # Assuming 'gtml' is a properly defined spatial object that needs to be projected
  # Check if 'gtml' is defined
  if (exists("st")) {
    # Project the GTML object to the same UTM projection
    st <- project(st, proj)
  } else {
    warning("GTML data is not defined.")
  }

  # IDW ---------------------------------------------------------------------
  # Extract extents from the GTML data
  x.range <- ext(st)[1:2]
  y.range <- ext(st)[3:4]

  # Generate a grid of points based on the extracted extents
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 1000),
                     y = seq(from = y.range[1], to = y.range[2], by = 1000))

  coordinates(grd) <- ~ x + y
  gridded(grd) <- TRUE
  raster::crs(grd) <- proj

  # Convert point data to 'SpatVector' if not already
  pnts <- methods::as(pnts, 'Spatial')

  # Perform IDW interpolation using gstat and convert to 'SpatRaster'
  if (temp == "tx") {
    idw_result <- idw(formula = tx ~ 1, locations = pnts, newdata = grd, idp = 2.0) # idp is the power parameter
  }else{
    idw_result <- idw(formula = tn ~ 1, locations = pnts, newdata = grd, idp = 2.0) # idp is the power parameter
  }

  idw_raster <- rast(idw_result)

  # Crop and mask the interpolated raster to the extent of the original GTML data
  idw_raster <- crop(idw_raster, st)
  idw_raster <- mask(idw_raster, st)
  # Project the raster to geographic coordinates if needed
  idw_raster <- project(idw_raster, crs(geog))


  df <- as.data.frame(idw_raster, xy = TRUE)

  ggplot(df, aes(x = .data$x, y = .data$y, fill = .data$var1.pred)) +  # use the correct variable name for your data
    ggplot2::geom_tile() +  # uses tiles to represent raster data
    ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1, type = "seq") +
    ggplot2::coord_fixed() +  # keeps the aspect ratio fixed
    ggplot2::labs(
      x = "Longitude\n", y = "Latitude\n",
      title = paste0("IDW Temperature Interpolation Map \n", reg),
      fill = "Temperature") +  # label for the legend
    theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face="bold", hjust = 0.5, size=12))




  plot(idw_raster)

  datatif <- file.path(paste0(datadir, "/tif"))
  dir.create(datatif, showWarnings = FALSE)
  writeRaster(idw_raster, filename = paste0(datatif, "/",
                                            reg, "_idwd.tif"), overwrite = TRUE)
}



# Install packages if not already installed

options(repos = c(CRAN = "https://cloud.r-project.org"))


if (!requireNamespace("devtools", quietly = TRUE))  install.packages("devtools")
if (!requireNamespace("geodata", quietly = TRUE))  remotes::install_github("rspatial/geodata")
if (!requireNamespace("tidyverse", quietly = TRUE))  install.packages("tidyverse")
if (!requireNamespace("gstat", quietly = TRUE))  install.packages("gstat")
if (!requireNamespace("sf", quietly = TRUE))  install.packages("sf")
if (!requireNamespace("rnaturalearth", quietly = TRUE))  devtools::install_github("ropensci/rnaturalearth")
if (!requireNamespace("raster", quietly = TRUE))  install.packages("raster")
if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) devtools::install_github("ropensci/rnaturalearthdata")


# Load libraries ----------------------------------------------------------
library(geodata)
library(tidyverse)
library(gstat)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(raster)

