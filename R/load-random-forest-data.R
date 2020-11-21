################################################################################
#' Load a single .csv file and output a dplyr dataframe
#'
#' @param file_path path to .csv file to load.
#' @param ... other arguments to be used by the read.csv function
#' @importFrom dplyr as_tibble
#' @export
#'
load_a_single_csv <- function(file_path, ...)
{
    # Load .csv file
    data <- read.csv(file = file_path, ...)
    # Convert dataframe into a dplyr dataframe
    data <- as_tibble(data)
    # Return
    return(data)
}

################################################################################
#' Load a bunch of csv files into a single list
#'
#' This function loads a bunch of .csv files with identical structure and recurrent pattern
#' into a single list. Each .csv file is assumed to have a recurrent pattern in its name.
#'
#' @param main_folder_path path to the main folder
#' @param folder name of the folder containing the .csv files to load. A character.
#' @param starts_with initial part of the name of the .csv file. Each file is assumed to have a recurrent pattern
#' in its name where the only changing part is assumed to be the year. A character
#' @param npixels Number of pixels in each dataframe. (number of rows of data)
#' @param years years to load. A character vector.
#' @param must_have_variables a vector of variables that must be included in each .csv file. Character vector.
#' @importFrom dplyr as_tibble mutate %>%
#' @export
#'
load_all_csv <- function(main_folder_path, folder, starts_with, years = c("2011", "2012"), npixels = 5520, must_have_variables = c("lon", "lat"))
{
    # Loading filess

    load_file <- function(year, main_folder_path, folder, starts_with)
    {
        # Build path
        file_name <- paste0(main_folder_path, "/", folder, "/", starts_with, year, ".csv")
        # Load .csv
        df_loaded <- read.csv(file_name)
        # Convert into dplyr dataframe and add year
        df_loaded <- as_tibble(df_loaded) %>% mutate(year = as.numeric(year))
        # Return
        return(df_loaded)
    }

    tbls <- lapply(X = years, FUN = load_file, main_folder_path=main_folder_path, folder = folder, starts_with=starts_with)

    # Checks

    # Count number of rows for each dataframe
    nrows <- sapply(tbls, nrow)
    # 1. Check that each dataframe has 5520 rows (pixels)
    if( any(nrows != npixels) ){ stop(paste("Some dataframes contain more or less pixels than the selected value of", npixels, sep = " ")) }
    # 2. Check that each dataframe contains "lon" and "lat" (must_have_variables)
    names_df <- lapply(tbls, names)
    names_bool <- lapply(names_df, function(x) must_have_variables %in% x)
    if( any(!unlist(names_bool)) ){ stop("Some of the required variables are missing from the input .csv files") }

    # Return
    return(tbls)
}

################################################################################
#' Align longitude and latitude values.
#'
#' If reformat = TRUE a "brute force" reformat is performed to match each longitude and latitude
#' value to those in the reference dataframe reference_df.
#'
#' If shift = TRUE the values of longitude and latitude will be shifted. By default, longitude
#' and latitude are assumed to be shifted by a negligible amount and replaced with those in
#' the reference dataframe. If this is not the case, you can add the shift_amount argument to
#' set the amount of the shifting.
#'
#' If both reformat and shift are set to FALSE, the df_list is simply returned without any changes.
#'
#' @param df_list a list of dplyr dataframes. For each dataframe in the list, longitude and latitude will be
#' fixed.
#' @param reference_df a reference dataframe that must contain at least longitude and latitude. A dataframe.
#' @param variable variable to be used in the reformatting process. The values of this variable will
#' be reordered according to the new longitude and latitude values. Character.
#' @param reformat Should lon and lat be fixed from scratch? Boolean.
#' @param shift Should lon and lat be shifted? Boolean.
#' @param lon_lat_names name of longitude and latitude variables. A character vector.
#' Example: by default lon_lat_names = c("lon", "lat")
#' @param shift_amount a named list of two numbers. If longitude and latitude should be shifted, by how much should they be shifted?
#'  By default shift_amount is set to NULL. If shift amount is set to NULL, longitude and latitude in the dataframe are replaced
#'  by longitude and latitude in the reference dataframe. If you need to shift longitude and latitude by a fixed amount, simply set
#'  the argument shift amount like this: list(lon = 1, lat = 2). In this case, longitude is shifted by adding 1 unit and latitude is shifted
#'  by adding 2 units. Note that shift_amount is a list with specific names.
#' @export
#'
format_lon_lat_list <- function(df_list, reference_df, variable = NULL, reformat = FALSE, shift = FALSE, lon_lat_names = c("lon", "lat"), shift_amount = NULL)
{
    # Do lon and lat in the df list need to be reformatted or shifted?
    if(reformat)
    {
        # Reformat
        out <- lapply(df_list, format_lon_lat, reference_df, variable, lon_lat_names)
    }
    # Do they need to be shifted?
    else if(shift)
    {
        # Shift
        out <- lapply(df_list, shift_lon_lat, reference_df, lon_lat_names, shift_amount)
    }
    else
    {
        # Do nothing
        out <- df_list
    }
    # Return
    return(out)
}

################################################################################
#' Shift lon and lat
#'
#' Simply replace lon and lat with those in the reference dataframe
#'
#' @param dataframe a dplyr dataframe.
#' @param reference_df a reference dataframe that must contain at least longitude and latitude. A dataframe.
#' @param lon_lat_names name of longitude and latitude variables. A character vector.
#' Example: by default lon_lat_names = c("lon", "lat")
#' @param shift_amount a named list of two numbers. If longitude and latitude should be shifted, by how much should they be shifted?
#'  By default shift_amount is set to NULL. If shift amount is set to NULL, longitude and latitude in the dataframe are replaced
#'  by longitude and latitude in the reference dataframe. If you need to shift longitude and latitude by a fixed amount, simply set
#'  the argument shift amount like this: list(lon = 1, lat = 2). In this case, longitude is shifted by adding 1 unit and latitude is shifted
#'  by adding 2 units. Note that shift_amount is a list with specific names.
#' @export
#'
shift_lon_lat <- function(dataframe, reference_df, lon_lat_names = c("lon", "lat"), shift_amount = NULL)
{
    # Shift lon and lat
    lon_name <- lon_lat_names[1]
    lat_name <- lon_lat_names[2]

    # Calculate the difference between reference values and actual values
    lon_diff <- dataframe[[lon_name]] - reference_df[[lon_name]]
    lat_diff <- dataframe[[lat_name]] - reference_df[[lat_name]]

    # Check that pixel is shifted by the same amount. If this is not the case, then a shift cannot occur.
    if( (length(unique(lon_diff)) != 1) | (length(unique(lat_diff)) != 1) ){ stop("Either lon or lat is not shiftable") }

    # If no shift_amount is provided then replace lon and lat with those in the reference dataframe
    if(is.null(shift_amount))
    {
        dataframe[[lon_name]] <- reference_df[[lon_name]]
        dataframe[[lat_name]] <- reference_df[[lat_name]]
    }else
    {
        dataframe[[lon_name]] <- dataframe[[lon_name]] + shift_amount$lon
        dataframe[[lat_name]] <- dataframe[[lat_name]] + shift_amount$lat
    }

    # Return
    return(dataframe)
}

## Da ottimizzare.
################################################################################
#' Fix latitude and longitude
#'
#' A brute force approach is used to match latitude and longitude pairs and the corrisponding value of
#' variable.
#'
#' @param dataframe a dplyr dataframe.
#' @param reference_df a reference dataframe that must contain at least longitude and latitude. A dataframe.
#' @param variable variable to be used in the reformatting process. The values of this variable will
#' be reordered according to the new longitude and latitude values. Character.
#' @param lon_lat_names name of longitude and latitude variables. A character vector.
#' Example: by default lon_lat_names = c("lon", "lat")
#' @importFrom dplyr as_tibble
#' @export
#'
format_lon_lat <- function(dataframe, reference_df, variable, lon_lat_names = c("lon", "lat"))
{
    # Number of rows in the current dataframe. Should always equal 5520.
    number_of_rows <- nrow(dataframe)

    lon_name <- lon_lat_names[1]
    lat_name <- lon_lat_names[2]

    # Reference lat and lon
    lon_riferimento <- reference_df[[lon_name]]
    lat_riferimento <- reference_df[[lat_name]]

    # Lat and lon to be fixed
    lon_disordinata <- dataframe[[lon_name]]
    lat_disordinata <- dataframe[[lat_name]]

    # Variable to be matched
    valore <- dataframe[[variable]]

    # Idea: siccome ogni combinazione lon-lat e' unica, combino le coppie lon e lat in un'unica variabile.
    # df: dataframe con termine misto: lonlat valore
    # In df lonlat e' un character.
    df <- data.frame(misto = paste(lon_disordinata, lat_disordinata, sep = ""),
                     valore = valore)

    # New values
    nuovo_val <- NULL

    # Estremamente lento
    for(i in 1:number_of_rows)
    {
        #
        lon_true <- lon_riferimento[i]
        # Quale dei valori della nuova longitudine differisce di meno in valore assoluto da
        # quella di riferimento?
        differ_index <- which( abs(lon_disordinata - lon_true) == min( abs(lon_disordinata - lon_true)) )
        nuova_lon <- lon_disordinata[differ_index][1]

        lat_true <- lat_riferimento[i]
        differ_index <- which( abs(lat_disordinata - lat_true) == min( abs(lat_disordinata - lat_true)) )
        nuova_lat <- lat_disordinata[differ_index][1]

        # Incolla le lat e lon trovate, poi trova indice della riga dove ho la stessa coppia di valori
        # lon-lat nel dataframe originale. Assegna valore della variabile corrispondente al nuovo vettore.
        stringa <- paste(nuova_lon, nuova_lat, sep = "")
        index <- which(df$misto == stringa)
        nuovo_val[i] <- df$valore[index]
    }

    # Dataframe reformatted
    new_dataframe <- data.frame(lon = lon_riferimento,
                                lat = lat_riferimento,
                                valore = nuovo_val,
                                year = dataframe$year)
    # Set names
    names(new_dataframe) <- names(dataframe)
    # Convert to a dplyr dataframe
    new_dataframe <- as_tibble(new_dataframe)
    # Return
    return(new_dataframe)
}

################################################################################
#' Add id_pixel and kmeans groups from a reference dataframe. Make a left join.
#'
#' For each dataframe in the data_frame list, id_pixel and the group from the kmeans analysis
#' is added. Note that the reference dataframe must contain longitude, latitude (lon and lat),
#' id_pixel and groups.
#'
#' @param dataframe_list a list of dplyr dataframes
#' @param reference_dataframe reference dataframe containing lon, lat, id_pixel, and groups.
#' @param join_by variables to join by
#' @importFrom dplyr left_join
#' @export
#'
add_id_pixel_and_groups <- function(dataframe_list, reference_dataframe, join_by = c("lon","lat"))
{
    # left_join on each dataframe and the reference_dataframe
    out <- lapply(dataframe_list, function(x) left_join(x, reference_dataframe, by = join_by))
    # Return
    return(out)
}

################################################################################
#' Join data together
#'
#' Join multiple year data by lon, lat and year
#' Then join single year data by lon and lat
#'
#' @param multiple_year_data a list of lists of dataframes. Each dataframe should contain observations of a single variable in a certain year.
#' @param single_year_data a list of dplyr dataframes.
#' @param m_year_join_by variables to join by (multiple year dataframes)
#' @param s_year_join_by variables to join by (single_year_dataframes)
#' @importFrom dplyr bind_rows left_join %>%
#' @export
#'
join_data <- function(multiple_year_data, single_year_data, m_year_join_by = c("lon", "lat", "year"), s_year_join_by = c("lon", "lat"))
{
    # Stack each set of observations in a single dataframe
    multiple_year_data <- lapply(multiple_year_data, bind_rows)

    # Join multiple year dataframes together
    m_data <- multiple_year_data[[1]]
    if(length(m_data > 1))
    {
        for(i in 2:length(multiple_year_data))
        {
            m_data <- left_join(m_data, multiple_year_data[[i]], by = m_year_join_by)
        }
     }

    # Join multiple year dataframes together
    s_data <- single_year_data[[1]]
    if(length(single_year_data) > 1)
    {
        for(i in 2:length(single_year_data))
        {
            s_data <- left_join(s_data, single_year_data[[i]], by = s_year_join_by)
        }
    }

    # Join in a single dataframe
    data <- left_join(m_data, s_data, by = s_year_join_by)
    # Return
    return(data)
}

################################################################################
#' Filter out pixels that have no group assigned
#'
#' @param data a dplyr dataframe
#' @param group_name name of the variable containing the group information
#' @export
#'
keep_pixels_with_group <- function(data, group_name = "gruppo")
{
    filtered_df <- data[!is.na(data[group_name]),]
    return(filtered_df)
}
