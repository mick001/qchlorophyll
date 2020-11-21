################################################################################
#' Load .nc files that contain a time variable.
#'
#' Load all .nc files as a list in a given local path and resize them while loading.
#'
#' Note: these function is used to load .nc files containing a time variable.
#' They represent observations of a certain variable with a given frequency.
#' The variable "time" defines the name of the sampling frequency variable.
#' The function assumes that the data has a daily frequency.
#' If the frequency is monthly, please set the monthy variable to TRUE.
#'
#' Due to the size of such files, immediate cropping of the selected geographical area is performed
#' at load time. Note that the size of the area should be specified in the arguments lower_left_lon_lat
#' and upper_right_lon_lat. If these arguments are not specified, defaults will be used.
#'
#' Dates are assumed to begin from the 1st of January. If the dates are in the format "hours"
#' from a given date", please specify the starting date in the format "yyyy-mm-dd". Please note
#' that no other formats are accepted. See the parameter date_origin below.
#'
#' @param path Path where to .nc files are located. Example: /home/data. Character.
#' @param from starting year (included). Either a numeric or a character. Example: 2009.
#' @param to ending year(included). Either a numeric or a character. Example: 2010.
#' @param variables variables to be extracted from .nc file. A character vector. Defaults to c("qnet")
#' @param coordinates longitude and latitude names. Defaults to c("lon", "lat")
#' @param spare_coordinates Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
#' .nc file. In order to account this possibility, you can provide a set of spare names for both coordinates. Set by default
#' to be: c("longitude","latitude").
#' @param time_variable variable representing the frequency of the observations. Character. Defaults to "time".
#' @param lower_left_lat_lon lower left corner latitude and longitude of the selected area. Set by default.
#' @param upper_right_lat_lon upper right corner latitude and longitude of the selected area.  Set by default.
#' @param monthly whether data has a monthly or an annual frequency. Boolean. Set equal to TRUE if data has a monthly frequency.
#' Set to FALSE if frequency is annual. FALSE by default.
#' @param date_origin If the dates in the .nc files are the number of hours from a fixed origin, please specify origin date as
#' "yyyy-mm-dd". By default origin date is assumed to be 1st January of each year (the year is read from the file name).
#' @importFrom stringi stri_extract_last_regex
#' @return a list of dplyr dataframes
#' @export
#'
load_nc_with_time <- function(path, from = NULL, to = NULL, variables = c("qnet"), coordinates = c("lon", "lat"), spare_coordinates = c("longitude", "latitude"), time_variable = "time", lower_left_lat_lon = c(52.00, -65.00), upper_right_lat_lon = c(67.00, -42.00), monthly = FALSE, date_origin = NULL)
{
    # Load file names from path. Select only .nc files
    file_names <- list.files(path = path, pattern = "\\.nc$")
    # Select only files within the given year range
    files_to_load <- select_files_by_year(file_names, from = from, to = to)
    # Build path to each file
    files_to_load_path <- lapply(files_to_load, make_path, path)
    # Load each .nc file as a list of dataframe
    files_loaded <- lapply(files_to_load_path, recover_nc_data,
                           variables = variables,
                           coordinates = coordinates,
                           spare_coordinates = spare_coordinates,
                           time_variable = time_variable,
                           lower_left_lat_lon = lower_left_lat_lon,
                           upper_right_lat_lon = upper_right_lat_lon,
                           monthly = monthly,
                           date_origin = date_origin)
    # Set names
    names(files_loaded) <- stri_extract_last_regex(files_to_load, "\\d{4}")
    # Return a list of dataframes ready to be manipulated
    return(files_loaded)
}

################################################################################
#' Load nc files to be resized. Subfunction
#'
#' @param file_path Path where the file is located. Example: /home/data. Character.
#' @param variables variables to be extracted from .nc file. Character vector.
#' @param coordinates longitude and latitude names
#' @param spare_coordinates Spare names for coordinates. Variables such as longitude and latitude may be named differently in every
#' .nc file. In order to account this possibility, you can provide a set of spare names for both coordinates. Set by default
#' to be: c("longitude","latitude").
#' @param time_variable variable representing the frequency of the observations. Character.
#' @param monthly whether data has a monthly or an annual frequency. TRUE if data has a monthly frequency.
#' FALSE if frequency is annual. FALSE by default.
#' @param lower_left_lat_lon lower left corner latitude and longitude of the selected area.
#' @param upper_right_lat_lon upper right corner latitude and longitude of the selected area.
#' @param date_origin If the dates in the .nc files are hours from a fixed origin, please specify origin date as
#' "yyyy-mm-dd". By default origin date is assumed to be 1st January of each year (the year is read from the file name).
#' @importFrom stringi stri_extract_last_regex
#' @importFrom ncdf4 nc_open nc_close
#' @importFrom lubridate dmy ymd yday month year as_date
#' @importFrom dplyr %>% mutate as_tibble bind_cols filter select all_of
#' @importFrom rlang exprs
#' @return a dplyr dataframe
#' @export
#'
recover_nc_data <- function(file_path, variables, coordinates, spare_coordinates, time_variable, monthly, lower_left_lat_lon, upper_right_lat_lon, date_origin = NULL)
{
    ########################################
    # Load nc file data
    ########################################

    # Open file
    current_nc_file <- nc_open(file_path)
    # Check existance of coordinates and replace them with spare ones if needed.
    coordinates <- fix_coordinates(nc = current_nc_file, coordinates = coordinates, spare_coordinates = spare_coordinates)
    # Total variables to retrieve
    variables_to_get <- c(coordinates, variables, time_variable)
    # Load each variable in a list
    raw_data <- lapply(variables_to_get, load_variable_from_nc, nc = current_nc_file)
    # Set names for each variable
    names(raw_data) <- variables_to_get
    # Close file
    nc_close(current_nc_file)

    ######################################
    # Generate grid and add variables
    ######################################

    # Expand coordinates (lon and lat)
    data_grid <- raw_data[c(coordinates, time_variable)] %>% expand.grid()

    # Add variables to data grid.
    fun <- function(x) data.frame(as.vector((x), mode = "numeric"))
    variables_data <- lapply(raw_data[variables], fun)
    variables_df <- do.call(cbind, variables_data)
    names(variables_df) <- variables
    variables_df <- variables_df %>% as_tibble()
    data <- data_grid %>% as_tibble() %>% bind_cols(variables_df)

    ######################################
    # Fix longitude.
    temp <- which(data$lon > 180)
    data$lon[temp] <- data$lon[temp] - 360
    ######################################

    # Select only the area you're interested in
    data <- data %>%
        filter(lat >= lower_left_lat_lon[1] & lat <= upper_right_lat_lon[1] & lon >= lower_left_lat_lon[2] & lon <= upper_right_lat_lon[2])

    # Get year from file path
    year <- as.numeric(stri_extract_last_regex(file_path, "\\d{4}"))
    # Referemce date is 31-12-(year-1) since then adding the time_variable will lead to the exact year in the date
    ref_date <- dmy(paste("31-12", year - 1, sep = "-"))

    # Add id_date, month, year. Mutate call for month and select call
    d <- as.name("date")
    r_d <- as.name(ref_date)
    time_v <- as.name(time_variable)
    mutate_call <- exprs(id_date = yday(!!r_d + !!time_v),
                         month = month(!!r_d + !!time_v),
                         year = year(!!r_d + !!time_v),
                         date = !!r_d + !!time_v)

    # Select call
    select_call <- c(coordinates[1], coordinates[2], variables, "id_date", "month", "year", "date")

    # If data frequency is monthly, update mutate and select calls
    if(monthly)
    {
        mutate_call <- exprs(id_date = yday(!!r_d + !!time_v),
                             month = month(!!time_v),
                             year = year(!!r_d + !!time_v),
                             date = !!r_d + !!time_v)

        select_call <- c(coordinates[1], coordinates[2], variables, "month", "year", "date")
    }
    # If data is specified from an origin, then act accordingly (example: shtfl .nc data)
    if( !is.null(date_origin) )
    {
        # Add to data the date
        data <- data %>% mutate(origin = ymd(date_origin),
                                date = as.Date(time / 24, origin = origin))
        # Redefine the mutate calls
        d <- as.name("date")
        mutate_call <- exprs(id_date = yday(!!d),
                             month = month(!!d),
                             year = year(!!d),
                             date = !!d)
    }

    # Mutate and select variables
    data <- data %>%
                mutate(!!!mutate_call) %>%
                    select(all_of(select_call))

    # Print status info
    print(paste("Loaded: ", strsplit(file_path, "/")[[1]][ length(strsplit(file_path, "/")[[1]]) ] ))

    # Return data
    return(data)
}

################################################################################
#' Select files to load from a given range of years.
#'
#' @param file_names names of the files to filter
#' @param from starting year (included). Either a numeric or a character. Example: 2009
#' @param to ending year(included). Either a numeric or a character. Example: 2010
#' @importFrom stringi stri_extract_last_regex
#' @return a vector of filenames filtered by year
#' @export
#'
select_files_by_year <- function(file_names, from, to)
{
    # Extract year from file name and convert it to a numeric
    years <- sapply(file_names, function(x) as.numeric(stri_extract_last_regex(x, "\\d{4}")))

    # If neither from or to are supplied, select the whole range
    if(is.null(from) && is.null(to))
    {
        selected_years <- years
    }
    # If to is supplied, load all files up to the selected year (included)
    else if(is.null(from) && !is.null(to))
    {
        selected_years <- years[years <= to]
    }
    # If from is supplied, load all files from the selected year(included)
    else if(!is.null(from) && is.null(to))
    {
        selected_years <- years[years >= from]
    }
    # Else, (if both are supplied), load the files in the supplied range of years
    else
    {
        selected_years <- years[years <= to]
        selected_years <- selected_years[selected_years >= from]
    }

    # Get file names to recover
    files_picked <- names(selected_years)

    # Return
    return(files_picked)
}
