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
#'
#' @examples
#'
#' Load the data example
#'
#' file_path <- system.file("extdata", "temp.csv", package = "coriM")
#' file <- readr::read_csv(file_path, show_col_types = FALSE)
#'
#' Run the IDW function
#'
#' idw_inter(dat = file, sta = TRUE, cntr="peru",coun_cd = 'PE',
#'  alt = TRUE, reg = "Puno")
#'
#'
#' @export

idw_inter <- function(dat, sta, cntr, coun_cd, alt, reg){


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
  idw_result <- idw(formula = tx ~ 1, locations = pnts, newdata = grd, idp = 2.0) # idp is the power parameter
  idw_raster <- rast(idw_result)

  # Crop and mask the interpolated raster to the extent of the original GTML data
  idw_raster <- crop(idw_raster, st)
  idw_raster <- mask(idw_raster, st)
  # Project the raster to geographic coordinates if needed
  idw_raster <- project(idw_raster, crs(geog))

  df <- as.data.frame(idw_raster, xy = TRUE)

  # Plotting the data using ggplot2
  ggplot(df, aes(x = x, y = y, fill = var1.pred)) +  # use the correct variable name for your data
    geom_tile() +  # uses tiles to represent raster data
    coord_fixed() +  # keeps the aspect ratio fixed
    scale_fill_gradientn(colors = heat.colors(7),
                         values = scales::rescale(c(14, 16,18, 20, 22, 24, 26)),
                         breaks = c(14, 16,18, 20, 22, 24, 26),
                         labels = c("14°C", "16°C", "18°C", "20°C", "22°C", "24°C", "26°C"),
                         guide = guide_colourbar(title = "Temperature", title.position = "top",
                                                 barwidth = 0.5, barheight = 6)) +
    labs(fill = "Temperature") +  # label for the legend
    ggtitle("Puno Temperature Map") +  # adds a title
    theme_minimal()


  #------ Create directory CSV files
  datatif <- file.path(paste0(datadir, "/tif"))
  dir.create(datatif, showWarnings=FALSE)

  # Write the raster to file
  writeRaster(idw_raster, filename = paste0(datatif,"/",reg,"_idwd.tif"), overwrite = TRUE)


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
if (!requireNamespace("rnaturalearthhires", quietly = TRUE))  devtools::install_github("ropensci/rnaturalearthhires")

# Load libraries ----------------------------------------------------------
library(geodata)
library(tidyverse)
library(gstat)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(raster)


