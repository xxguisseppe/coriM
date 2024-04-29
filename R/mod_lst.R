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
#' @import ggsn
#' @import maptools
#' @import gridExtra
#' @import ggplot2
#' @import RColorBrewer
#'
#' @examples
#'
#' The example is for the Region of Puno, for the year 2010 in the month of March.
#'
#' mod_lst (sen = "Terra", usr = 'xxguisseppexx', pass = 'Arturo!23456',
#'           bd = "2010.03.01", ed = "2010.03.31", mnth = "March",
#'           proj = 4326, cntr = "peru",sta = TRUE, reg = "Puno")
#'
#'
#' @export
#'

#sf::sf_extSoftVersion()

mod_lst  <- function(sen, usr, pass, bd, ed, mnth, proj, cntr, sta, reg){

  #------ Create directory MODIS and tif
  datadir <- file.path(paste0(getwd(), "/MODIS"))
  dir.create(datadir, showWarnings=FALSE)

  datatif <- file.path(paste0(datadir, "/tif"))
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
  mf <- MODIStsp::MODIStsp(gui = FALSE, out_folder = datadir,
                           selprod = "Surf_Temp_8Days_1Km (M*D11A2)",
                           bandsel = "LST_Day_1km", sensor = sen,
                           user = 'Guisseppe',
                           password = "Arturo!23456",
                           start_date = bd, end_date = ed,
                           out_format = "GTiff", delete_hdf = TRUE,
                           spatmeth = "bbox",
                           bbox = numeric_coordinates, output_proj = 4326)


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



  raster_df <- as.data.frame(cropped_raster, xy = TRUE, na.rm = TRUE)

  # Plotting the data using ggplot2
  a <- min(raster_df$x); b <- max(raster_df$x)
  c <- min(raster_df$y); d <- max(raster_df$y)
  rango <- round(seq(from = min(raster_df$layer), to = max(raster_df$layer), length.out = 8),0)

  col_pal <- "YlOrRd"
  cbar <- plot_discrete_cbar(rango,
                             spacing = "constant", font_size = 4, expand_size = 0.2,
                             palette = col_pal, legend_direction="horizontal", legend_title="Temperature [°C]", border_color="black")

  # Plotting the data using ggplot2
  pl <- ggplot(raster_df, aes(x = x, y = y, fill = layer)) +  # use the correct variable name for your data
    geom_tile() +  # uses tiles to represent raster data
    coord_cartesian(
      xlim = c(a,b),
      ylim = c(c,d)) +
    scale_fill_distiller(palette = col_pal, direction = 1, type = "seq") +
    labs(
      x = "Longitude [°]\n", y = "Latitude [°]\n", title = paste0("MODIS Temperature Map \n", reg)) +
    guides(
      fill = guide_colourbar(order = 1)) +
    guides(fill = "none") +
    theme_classic() +
    theme(
      plot.title = element_text(face="bold", hjust = 0.5, size=12),
      #legend.position = "none",
      legend.position="bottom",
      legend.box = "horizontal",
      legend.title = element_text(colour="black", size=9, face="bold", hjust = 0.5),
      legend.text = element_text(colour="black", size=7),
      legend.key.height = unit(0.3, "in"),
      legend.background = element_rect(fill="#e6ffff", linewidth=0.5, linetype="solid", colour ="black"),
      axis.title.x = element_text(color="black", size=10, face="bold", hjust = 0.5),
      axis.title.y = element_text(color="black", size=10, face="bold", hjust = 0.5 ),
      plot.margin = unit(c(20,40,0,20), "pt")
    )

  cbar <- cbar + theme(plot.margin = unit(c(10, 5, 50, 20), "pt"))

  png(filename =  paste0(getwd(), "/",reg,"_MODIS_LST.png"), width = 2020, height = 3260, units = "px", pointsize = 9, res = 350 )
  pm <- gridExtra::grid.arrange(pl, cbar, nrow=2, heights=c(7, 1.5))
  print(pm)
  dev.off()





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
if (!requireNamespace("maptools", quietly = TRUE)) install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
if (!requireNamespace("ggsn", quietly = TRUE))  devtools::install_github('oswaldosantos/ggsn')
if (!requireNamespace("gridExtra", quietly = TRUE))  install.packages("gridExtra")


# Load Libraries
library(terra)
library(MODIStsp)
library(sf)
library(rnaturalearth)
library(sp)
library(raster)
library(maptools)
#library(ggsn)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)

