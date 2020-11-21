################################################################################
#' Load the mask file as a data frame.
#'
#' @param mask_file path to nc file containing the binary mask
#' @param lon_lat_names names of coordinates in the mask file. Defaults to c("lon", "lat")
#' @param mask_name mask variable name
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom dplyr as_tibble %>%
#' @return a dplyr dataframe
#' @export
#'
load_mask <- function(mask_file = "mask_25km.nc", lon_lat_names = c("lon", "lat"), mask_name = "lsmask")
{
    # Load mask
    mask_nc <- nc_open(mask_file)
    lon <- ncvar_get(mask_nc, lon_lat_names[1])
    lat <- ncvar_get(mask_nc, lon_lat_names[2])
    md <- ncvar_get(mask_nc, mask_name)
    nc_close(mask_nc)

    # Reset longitude
    temp <- which(lon > 180)
    lon[temp] <- lon[temp] - 360

    # Load data as dataframe
    mask <- expand.grid(lon = lon, lat = lat)
    mask$mask <- unlist(as.vector(md))
    mask <- mask %>% as_tibble()

    # Return mask as a dataframe
    return(mask)
}

################################################################################
#' Resize the mask
#'
#' @param mask a mask dataframe loaded using the load_mask function.
#' @param reference_df a reference dataframe containing at least the following variables: longitude, latitude, id_pixel.
#' @param step step (resolution) in the longitude-latitude grid. Set to 0.25 by default.
#' @param coordinates_names names of the coordinates (longitude and latitude).
#' @param plot_ Should the function plot the original and interpolated map for a visual comparison? Boolean. FALSE by default.
#' @importFrom dplyr filter %>%
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradient ggtitle
#' @importFrom sp coordinates gridded
#' @importFrom gstat idw
#' @return a dplyr dataframe
#' @export
#'
resize_mask <- function(mask, reference_df, step=0.25, coordinates_names = c("lon", "lat"), plot_ = FALSE)
{
    # Determine lon and lat range and step
    lon_range <- as.numeric(c(min(reference_df[coordinates_names[1]]), max(reference_df[coordinates_names[1]])))
    lat_range <- as.numeric(c(min(reference_df[coordinates_names[2]]), max(reference_df[coordinates_names[2]])))

    # Filter selected area
    mask <- mask %>%
        filter(lat >= lat_range[1] & lat <= lat_range[2] & lon >= lon_range[1] & lon <= lon_range[2])
    # Save a copy for later
    df_1 <- mask

    # Set coordinates
    sp::coordinates(mask) <- ~ lon + lat

    # Generate grid
    grd <- expand.grid(lon = seq(from = lon_range[1], to = lon_range[2], by = step),
                       lat = seq(from = lat_range[1], to = lat_range[2], by = step))
    # Set coordinates
    sp::coordinates(grd) <- ~ lon + lat
    gridded(grd) <- TRUE

    # Interpolate spatial data with idw model
    idw <- idw(formula = mask ~ 1, locations = mask, newdata = grd, nmax = 2)
    idw.output = as.data.frame(idw)
    names(idw.output)[1:3] <- c("lon", "lat", "mask_interp")

    # Interpolated with binary
    out_mask <- idw.output
    out_mask$mask_interp <- ifelse(out_mask$mask_interp > 0.5, 1, 0)

    # If plot_ == TRUE then plot the two masks
    if(plot_)
    {
        # Original
        gg1 <- ggplot() + geom_tile(data = out_mask, aes(x = lon, y = lat, fill=mask_interp)) +
            scale_fill_gradient(low = "white", high = "steelblue") +
            ggtitle("Original mask")

        # Interpolated
        gg2 <- ggplot() + geom_tile(data = df_1, aes(x = lon, y = lat, fill=mask)) +
            scale_fill_gradient(low = "white", high = "steelblue") +
            ggtitle("Interpolated mask")

        # Print plots
        multiplot(plots = list(gg1, gg2), 2)
    }

    # Return interpolated mask
    return(out_mask)
}

################################################################################
#' Apply the mask to the data and remove earth pixels
#'
#' @param data data to be used. A data frame object.
#' @param mask a mask dataframe loaded using the load_mask function or the resize_mask function.
#' @importFrom dplyr filter %>% semi_join
#' @return a dplyr dataframe
#' @export
#'
apply_mask <- function(data, mask)
{
    mask <- mask %>% filter(mask_interp == 1)
    out_data <- data %>% semi_join(mask, by = c("lon", "lat"))
    return(out_data)
}
