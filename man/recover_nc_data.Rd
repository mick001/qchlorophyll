% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load-data-with-time-variable.R
\name{recover_nc_data}
\alias{recover_nc_data}
\title{Load nc files to be resized. Subfunction}
\usage{
recover_nc_data(
  file_path,
  variables,
  coordinates,
  spare_coordinates,
  time_variable,
  monthly,
  lower_left_lat_lon,
  upper_right_lat_lon,
  date_origin = NULL
)
}
\arguments{
\item{file_path}{Path where the file is located. Example: /home/data. Character.}

\item{variables}{variables to be extracted from .nc file. Character vector.}

\item{coordinates}{longitude and latitude names}

\item{spare_coordinates}{Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
.nc file. In order to account this possibility, you can provide a set of spare names for both coordinates. Set by default
to be: c("longitude","latitude").}

\item{time_variable}{variable representing the frequency of the observations. Character.}

\item{monthly}{whether data has a monthly or an annual frequency. TRUE if data has a monthly frequency.
FALSE if frequency is annual. FALSE by default.}

\item{lower_left_lat_lon}{lower left corner latitude and longitude of the selected area.}

\item{upper_right_lat_lon}{upper right corner latitude and longitude of the selected area.}

\item{date_origin}{If the dates in the .nc files are hours from a fixed origin, please specify origin date as
"yyyy-mm-dd". By default origin date is assumed to be 1st January of each year (the year is read from the file name).}
}
\value{
a dplyr dataframe
}
\description{
Load nc files to be resized. Subfunction
}
