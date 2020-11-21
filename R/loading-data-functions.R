################################################################################
#' Load all .nc files in a given local path
#'
#' @param path Path where to .nc files are located. Example: /home/data. Character.
#' @param from Starting date to load files. Character. Must be in the same format selected with the parameter date_format.
#' By default "from" is NULL.
#' @param to Ending date to load files. Character. Must be in the same format selected with the parameter date_format.
#' By default "to" is NULL.
#' @param variables variables to be retrieved from the .nc file. A character vector of length n. Set by default to be: c("CHL1_mean").
#' @param coordinates Grid variables (for instance, longitude and latitude). Set by default to be: c("lon","lat").
#' @param spare_coordinates Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
#' .nc file. In order to account this possibility, you can provide a set of spare names for both coordinates. Set by default
#' to be: c("longitude","latitude").
#' @param date_match_position An integer. If the name of each file contains one or more dates, chose which one will be used as the
#' current date for the file. Example: if a file is named "20150202file2_20150706.nc", if date_match_position is set
#' to 1 (default), the first date, 20150202 will be used. If you'd like to use the second one, set the parameter to 2 and so on.
#' @param date_format date format. By default it is set to be "ymd" (year, month, day). This is the format of the date in each
#' filename. It can be set to other values such as "dmy" and "mdy". Check lubridate's package help for more information.
#' @return A list of all the .nc files loaded
#' @export
#'
load_all_as_list <- function(path, from = NULL, to = NULL, variables = c("CHL1_mean"), coordinates = c("lon", "lat"), spare_coordinates = c("longitude", "latitude"), date_format = "ymd", date_match_position = 1)
{
    # List files with extension .nc in the given path
    file_names <- list.files(path = path, pattern = "\\.nc$")

    # Seleziona solo i file nell'intervallo di date
    selected_file_names <- select_filenames_by_date(file_names = file_names,
                                                    from = from,
                                                    to = to,
                                                    date_format = date_format)

    # Genera il percorso al singolo file (cfr utils.R)
    nc_files_path <- sapply(selected_file_names, make_path, path = path)

    # Carica tutti i file in una singola lista.
    # Ogni elemento della lista contiene un dataframe dplyr
    nc_files <- lapply(nc_files_path, FUN = load_nc_file,
                                            variables,
                                            coordinates,
                                            spare_coordinates,
                                            date_format,
                                            date_match_position)

    # Ritorna la lista di tutti i file caricati
    return(nc_files)
}

################################################################################
#' Load a single .nc file
#'
#' This function loads a single .nc files and returns a dplyr dataframe of the extracted variables.
#'
#' @param file_path the path of the file to load. Character.
#' @param variables variables to be retrieved from the file. A character vector of length n.
#' @param coordinates longitude and latitude
#' @param spare_coordinates Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
#' .nc file. In order to account this possibility, you can provide a set of spare names for both coordinates.
#' @param date_format the format of the dates for each file.
#' @param date_match_position An integer. If the name of each file contains one or more dates, chose which one will be used as the
#' current date for the file. Example: if a file is named "20150202file2_20150706.nc", if date_match_position is set
#' to 1 (default), the first date, 20150202 will be used. If you'd like to use the second one, set the parameter to 2.
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @return A dplyr dataframe
#' @examples
#' # Load "data02022015.nc"
#' # data_02022015 <- load_nc_file("~path/data02022015.nc", date_format = "dmy")
#' @export
#'
load_nc_file <- function(file_path, variables = c("CHL1_mean"), coordinates = c("lon", "lat"), spare_coordinates = c("longitude", "latitude"), date_format = "ymd", date_match_position = 1)
{
    ###################
    # Retrieving data
    ###################

    # Extract current date from filepath(cfr utils.R)
    current_date <- extract_date_from_filepath(file_path,
                                               date_format = date_format,
                                               date_match_position = date_match_position)
    print(current_date)

    # Open file
    current_nc_file <- nc_open(file_path)

    # Check existance of coordinates and replace them with spare ones if needed.
    coordinates <- fix_coordinates(nc = current_nc_file, coordinates = coordinates, spare_coordinates = spare_coordinates)

    # Total variables to retrieve
    variables_to_get <- c(coordinates, variables)

    # Load each variable into a list. (cfr utils.R)             # Memo: after FUN, in lapply, arguments are passed to FUN
    raw_data <- lapply(variables_to_get, load_variable_from_nc, nc = current_nc_file)

    # Set names for each variable
    names(raw_data) <- variables_to_get

    # Close file
    nc_close(current_nc_file)

    # Reshape data. Basically: melt everything in a single dplyr dataframe.
    # Notes: each pixel is uniquely identified by latitude and longitude. For
    # each pixel a single measurement is performed.
    # e.g.
    # pixel 1, 2, 3 are identified by l1 lg1, l2 lg2, l3 lg3. Then measurements
    # m1, m2 and m3 are sampled.
    #
    # The melted (reshaped) df should look like this
    # lat long id_pixel meas
    # l1  lg1  1        m1
    # l2  lg2  2        m2
    # ...
    #
    data_reshaped <- reshape_data(raw_data = raw_data,
                                  variables = variables,
                                  expand_variables = coordinates,
                                  current_date = current_date)

    return(data_reshaped)
}


################################################################################
#' Reshape the raw data and output into a dplyr dataframe.
#'
#' @param raw_data raw data extracted using load_nc_file. A list.
#' @param variables variables to be retrieved from the file. A character vector of length n.
#' @param expand_variables variables to be used as x and y reference of the image grid. These variables must be included in the raw_data list.
#' @param current_date The date of the observation.
#' @importFrom lubridate day month year yday
#' @importFrom dplyr mutate bind_cols as_tibble %>%
#' @return A dplyr dataframe
#' @export
#'
reshape_data <- function(raw_data, variables, expand_variables, current_date)
{
    ####################################
    # Reshaping.
    ####################################

    # Expand coordinates (lon and lat)
    data_grid <- raw_data[expand_variables] %>% expand.grid()

    # Convert data.frame object to a tibble
    data_grid <- as_tibble(data_grid)

    # Melt variables into a single dataframe.
    #
    # Note: variables (measurements) are retrieved as 60*92 matrices
    # fun melts each 60x92 matrix into a dataframe.
    # Then the dataframes are binded by column
    #
    fun <- function(x) data.frame(as.vector((x), mode = "numeric"))
    variables_data <- lapply(raw_data[variables], fun)
    variables_df <- do.call(cbind, variables_data)
    names(variables_df) <- variables

    # Assign variables to a reshaped
    # Note: lubridate::yday() ritorna giorno dell'anno 1-366. Sostituito a as.POSIXlt(date)$yday che ritorna il giorno dell'anno 0-365
    # (ecco da dove spunta il - 1). Si potrebbe anche togliere. Attendo conferma. Fonti:
    # https://stat.ethz.ch/R-manual/R-devel/library/base/html/DateTimeClasses.html
    # http://www.inside-r.org/packages/cran/lubridate/docs/yday
    #
    reshaped_data <- data_grid %>%
        mutate(date = current_date) %>%
        bind_cols(variables_df) %>%
        mutate(id_date = yday(date) - 1,
               month = month(date),
               year = year(date))

    return(reshaped_data)
}


################################################################################
#' Given a list of data loaded using the load_all_as_list() function, this function is used to
#' assign a unique id to each pixel and bind all the rows together in a single dataframe.
#'
#' @param data_list A list of dplyr dataframes returned by the load_all_as_list function.
#' @param coordinates Unique identifier to be used in the id assignment process.
#' @importFrom dplyr bind_rows %>% select mutate row_number full_join distinct
#' @return A dplyr dataframe
#' @export
#'
assign_id_and_melt <- function(data_list, coordinates =  c("lon", "lat"))
{

    # Bind all rows in a single dataframe
    data <- data_list %>% bind_rows()

    # Calculate id_pixel
    id <- data %>%
        select(coordinates[2], coordinates[1]) %>%
        distinct() %>%
        mutate(id_pixel = row_number())

    # Add id_pixel
    data <- full_join(id, data, by = coordinates)

    return(data)
}
