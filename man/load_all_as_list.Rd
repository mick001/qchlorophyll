% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loading-data-functions.R
\name{load_all_as_list}
\alias{load_all_as_list}
\title{Load all .nc files in a given local path}
\usage{
load_all_as_list(
  path,
  from = NULL,
  to = NULL,
  variables = c("CHL1_mean"),
  coordinates = c("lon", "lat"),
  spare_coordinates = c("longitude", "latitude"),
  date_format = "ymd",
  date_match_position = 1
)
}
\arguments{
\item{path}{Path where to .nc files are located. Example: /home/data. Character.}

\item{from}{Starting date to load files. Character. Must be in the same format selected with the parameter date_format.
By default "from" is NULL.}

\item{to}{Ending date to load files. Character. Must be in the same format selected with the parameter date_format.
By default "to" is NULL.}

\item{variables}{variables to be retrieved from the .nc file. A character vector of length n. Set by default to be: c("CHL1_mean").}

\item{coordinates}{Grid variables (for instance, longitude and latitude). Set by default to be: c("lon","lat").}

\item{spare_coordinates}{Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
.nc file. In order to account this possibility, you can provide a set of spare names for both coordinates. Set by default
to be: c("longitude","latitude").}

\item{date_format}{date format. By default it is set to be "ymd" (year, month, day). This is the format of the date in each
filename. It can be set to other values such as "dmy" and "mdy". Check lubridate's package help for more information.}

\item{date_match_position}{An integer. If the name of each file contains one or more dates, chose which one will be used as the
current date for the file. Example: if a file is named "20150202file2_20150706.nc", if date_match_position is set
to 1 (default), the first date, 20150202 will be used. If you'd like to use the second one, set the parameter to 2 and so on.}
}
\value{
A list of all the .nc files loaded
}
\description{
Load all .nc files in a given local path
}
