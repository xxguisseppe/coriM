#' Function to check all the regions in the console and selected
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
  rr <- cat(paste("Select region:  ", st$name_en), sep='\n')
  print(rr)
  return(rr)
}




library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
