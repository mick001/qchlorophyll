% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolating-functions.R
\name{interpolate_grid}
\alias{interpolate_grid}
\title{Interpolate a list of dplyr dataframes}
\usage{
interpolate_grid(
  data_list,
  reference_df,
  variable,
  unique_id = "id_date",
  step = 0.25,
  coordinates_names = c("lon", "lat"),
  date_variable_name = "date"
)
}
\arguments{
\item{data_list}{a list of dplyr dataframes to interpolate}

\item{reference_df}{a reference dataframe containing at least the following variables: longitude, latitude, id_pixel.}

\item{variable}{variable to interpolate. Character.}

\item{unique_id}{unique id of the observation (usually id_date for daily observations or id_month for monthly observations).
Do NOT use id_pixel. Here is why:
this id should identify the a set of observations at a given time since the interpolation is done on a daily basis using the
values of all the available pixels for that day. In short, the id should identify a specific time, not a specific pixel.
Furthermore, the id id_pixel loses its meaning when changing the resolution of the data.}

\item{step}{step (resolution) in the longitude-latitude grid. Set to 0.25 by default.}

\item{coordinates_names}{names of the coordinates (longitude and latitude).}

\item{date_variable_name}{name of the date variable. Character. Defaults to "date"}
}
\value{
a dplyr dataframe
}
\description{
Interpolate a list of dplyr dataframes
}
