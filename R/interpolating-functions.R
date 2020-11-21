################################################################################
#' Interpolate a list of dplyr dataframes
#'
#' @param data_list a list of dplyr dataframes to interpolate
#' @param reference_df a reference dataframe containing at least the following variables: longitude, latitude, id_pixel.
#' @param variable variable to interpolate. Character.
#' @param unique_id unique id of the observation (usually id_date for daily observations or id_month for monthly observations).
#' Do NOT use id_pixel. Here is why:
#' this id should identify the a set of observations at a given time since the interpolation is done on a daily basis using the
#' values of all the available pixels for that day. In short, the id should identify a specific time, not a specific pixel.
#' Furthermore, the id id_pixel loses its meaning when changing the resolution of the data.
#' @param step step (resolution) in the longitude-latitude grid. Set to 0.25 by default.
#' @param coordinates_names names of the coordinates (longitude and latitude).
#' @param date_variable_name name of the date variable. Character. Defaults to "date"
#' @importFrom dplyr bind_rows
#' @return a dplyr dataframe
#' @export
#'
interpolate_grid <- function(data_list, reference_df, variable, unique_id = "id_date", step = 0.25, coordinates_names = c("lon", "lat"), date_variable_name = "date")
{
    # Determine lon and lat range and step
    lon_range <- as.numeric(c(min(reference_df[coordinates_names[1]]), max(reference_df[coordinates_names[1]])))
    lat_range <- as.numeric(c(min(reference_df[coordinates_names[2]]), max(reference_df[coordinates_names[2]])))

    # Print status info
    print(paste("Starting interpolation process. Interpolating", length(data_list), "file(s). This may take a while...", sep = " "))

    # Interpolate each dataframe in the list
    fitted_df_list <- lapply(data_list, FUN = interpolate_single_grid_multi_day,
                             unique_id = unique_id,
                             variable = variable,
                             coordinates_names = coordinates_names,
                             lon_range = lon_range,
                             lat_range = lat_range,
                             step = step,
                             date_variable_name = date_variable_name)

    # Print status
    print("Done interpolating data frames. Stacking data frames row wise...")
    # Stack all the dataframes in a single dataframe
    fitted_df <- bind_rows(fitted_df_list)
    # Done
    print("Done!")
    # Return
    return(fitted_df)
}

################################################################################
#' Interpolate each data grid.
#' This function interpolates every datagrid day by day (or month by month in case
#' of monthly observations)
#'
#' @param df a dplyr dataframe to interpolate
#' @param unique_id unique id of the observation (usually id_date or month). Do NOT use id_pixel.
#' @param variable variable to interpolate
#' @param coordinates_names names of the coordinates (longitude and latitude)
#' @param lon_range longitude range (min, max)
#' @param lat_range latitude range (min, max)
#' @param step step between longitude and latitude vectors
#' @param date_variable_name name of the date variable. Character. Defaults to "date"
#' @importFrom dplyr %>% select distinct filter mutate as_tibble bind_rows first all_of bind_cols
#' @importFrom sp coordinates gridded
#' @importFrom gstat idw
#' @importFrom lazyeval interp
#' @return a dplyr dataframe
#' @export
#'
interpolate_single_grid_multi_day <- function(df, unique_id, variable, coordinates_names, lon_range, lat_range, step, date_variable_name)
{
    # Count the number of days/month to interpolate
    days <- df %>% select(unique_id) %>% distinct() %>% unlist()
    names(days) <- NULL
    # Output list
    out_lst <- list()

    # Set new grid, coordinates and gridded attribute
    grd <- expand.grid(lon = seq(from = lon_range[1], to = lon_range[2], by = step), lat = seq(from = lat_range[1], to = lat_range[2], by = step))
    sp::coordinates(grd) <- ~ lon + lat
    sp::gridded(grd) <- TRUE
    # idw formula
    idw_formula <- as.formula(paste(variable, 1, sep=" ~ "))

    # Start to interpolate each date
    for(i in days)
    {
        # Select data from the ith date
        day_data <- df %>% filter(.data[[unique_id]] == i) %>% na.omit()

        # Set aside date for later mutate call
        day_date <- day_data %>% select( {{date_variable_name}} ) %>% distinct() %>% first()
        # Delete date from df
        day_data <- day_data %>% select( !{{date_variable_name}} )

        # Set aside variables common to each date (id_date, month, year)
        other_vars_name <- setdiff(names(day_data), c(coordinates_names, variable))
        other_vars <- day_data %>%
            select(all_of(other_vars_name)) %>%
            distinct() %>%
            as.list()

        # Set coordinates
        sp::coordinates(day_data) = ~ lon + lat
        # Interpolate using idw
        idw_out <- idw(formula = idw_formula, locations = day_data, newdata = grd, debug.level = 0)

        # Formatting output as dataframe
        idw_output <- as.data.frame(idw_out)
        # Set names
        names(idw_output)[1:3] <- c(coordinates_names[1], coordinates_names[2], variable)
        # Format output
        idw_output <- as_tibble(idw_output) %>%
            select(coordinates_names[1], coordinates_names[2], variable) %>%

            ###########################################################################################
            ###########################################################################################
            bind_cols(other_vars) %>%
            ###########################################################################################
            ###########################################################################################

            mutate(date = day_date)
        # Assign output to list
        out_lst[[i]] <- idw_output
    }
    # Bind all the dataframes in a single dataframe
    df_out <- bind_rows(out_lst)
    # Print status
    print("Done interpolating current file.")
    # Return
    return(df_out)
}
