% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_lst.R
\name{mod_lst}
\alias{mod_lst}
\title{Function that downloads 8-day composite MODIS LST data.
The monthly average is generated depending on the chosen date.}
\usage{
mod_lst(sen, usr, pass, bd, ed, mnth, proj, cntr, sta, reg)
}
\arguments{
\item{sen}{a character vector of MODIS Sensor name "Terra, Aqua".}

\item{usr}{a character vector of USERNAME of Earthdata Login.}

\item{pass}{a character vector of PASSWORD of Earthdata Login.}

\item{bd}{a character vector, format YYYY.MM.DD, of Start date}

\item{ed}{a character vector, format YYYY.MM.DD, of End date}

\item{mnth}{a character vector of Month, "Jan, Feb, March, ..."}

\item{proj}{a numeric vector, ESPG Projection number}

\item{cntr}{a character vector, NAME of the country}

\item{sta}{a LOGIC vector, Load Country or Region Boundaries}

\item{reg}{a character vector, name of the Region of the Country}
}
\description{
The output is a "GTiff" file format for the selected Country or Region.
}
