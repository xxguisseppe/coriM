#' This function will show me in console, all the regions of the country
#' selected for the calculation in case someone wants to get information of
#' a more precise area.
#'
#' @param cntry character vector country name
#'
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @import rnaturalearthhires
#'
#' @examples
#'
#' selCountry(cntry = "peru")
#'
#' @export


selCountry <- function(cntry){
  st <- ne_states(country = cntry, returnclass = "sv")
  cat(paste("Region:", st$name_en), sep='\n')
}


# Load libraries

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
