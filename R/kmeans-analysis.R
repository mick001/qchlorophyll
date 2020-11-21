################################################################################
#' Impute NAs using mice.
#'
#' This function is used to fill missing NA values using common imputation methods.
#' The imputation method can be chosen by setting the meth parameter. For a list of
#' all the available imputation methods available, please check the documentation of
#' the mice::mice function (?mice::mice)
#'
#' @param data a dplyr dataframe containing the data to be standardized
#' @param seed random seed for reproducibility purposes
#' @param exclude_variables variables to be excluded from the standardization process. A vector of characters.
#' @param m Number of multiple imputations. The default is m=5.
#' @param maxit A scalar giving the number of iterations. The default is 5
#' @param meth method used for imputation. See mice::mice function description for more information.
#' @param verbose Be verbose?
#' @param ... other parameters passed directly to the mice::mice function
#' @importFrom dplyr select bind_cols as_tibble %>% all_of
#' @return A dplyr dataframe
#' @export
#'
approximate_NAs <- function(data, seed, exclude_variables = c("lon", "lat", "id_pixel"), m = 5, maxit = 50, meth = "pmm", verbose = FALSE, ...)
{
    requireNamespace("mice")

    # Drop columns not needed
    data_to_fill <- data %>% select(!all_of(exclude_variables))

    # Impute data using mice
    tempData <- mice::mice(data_to_fill, m = m, maxit = maxit, meth = meth, seed = seed, printFlag = verbose, ...)

    # Filled data.
    filled_data <- mice::complete(tempData, 1)
    filled_data <- as_tibble(filled_data)

    # Bind excluded columns
    filled_data <- data %>% select(all_of(exclude_variables)) %>% bind_cols(filled_data)

    # Set attribute for future use. Check no slowdowns
    attr(filled_data, "imputed_dataset") <- tempData

    # Return
    return(filled_data)
}

################################################################################
#' Plot the density of imputed NAs vs actual data
#'
#' Density plot of imputed NAs vs actual data.
#'
#' @param data dataset imputed obtained from the approximate_NAs function,
#' must have an "imputed_dataset" attribute.
#' @importFrom lattice densityplot stripplot
#' @export
#'
plot_density_imputed_na <- function(data)
{
    # Mice imputed dataset
    tempData <- attr(data, "imputed_dataset")
    # First density plot
    density_plot_1 <- densityplot(tempData)
    # Second density plot
    density_plot2 <- stripplot(tempData)
    # Print the first plot
    print(density_plot_1)
    # Wait for user input
    readline(prompt = "Press anykey for next plot... It may take a while if there's a lot of data...")
    # Print the second plot
    print(density_plot2)
}

################################################################################
#' Standardize data before running kmeans
#'
#' This function is used to standardize the data before running the kmeans analysis function.
#' The output of this function is a dataframe ready to be used for the kmeans analysis.
#'
#' @param data a dplyr dataframe containing the data to be standardized
#' @param exclude_variables variables to be excluded from the standardization process. A vector of characters.
#' @importFrom dplyr select %>% as_tibble bind_cols all_of
#' @return A dplyr dataframe
#' @export
#'
standardize_data <- function(data, exclude_variables = c("lon", "lat", "id_pixel"))
{
    # Drop columns not needed
    data_to_scale <- data %>% select(!all_of(exclude_variables))

    # Standardize variables by their range
    rge <- sapply(data_to_scale, function(x) diff(range(x)))
    scaled_data <- sweep(x = data_to_scale, MARGIN = 2, STATS = rge, FUN = "/")
    scaled_data <- as_tibble(scaled_data)

    # Bind columns
    scaled_data <- data %>% select(all_of(exclude_variables)) %>% bind_cols(scaled_data)

    # Attach rge for later use
    attr(scaled_data, "rge") <- rge

    # Return
    return(scaled_data)
}

################################################################################
#' Run kmeans analysis
#'
#' Note: this function takes as argument all the arguments used by the stats::kmeans function.
#'
#' @param x a dataframe or a matrix containing the data. The data should be
#' standardized for better results
#' @param n_centers Number of clusters to be used. A vector such as 2:5
#' @param nstart how many random sets should be chosen?
#' @param seed Random seed set for reproducibility purposes. Numeric, NULL by default. (Integer)
#' @param exclude_variables variables to exclude from kmeans analysis. A vector of characters
#' @param ... Other arguments accepted by the stats::kmeans function.
#' @importFrom clusterSim index.G1
#' @importFrom dplyr select all_of
#' @return An object of class kmeans
#'
#' @export
#'
kmeans_analysis <- function(x, n_centers, nstart = 1, seed = NULL, exclude_variables = c("lon", "lat", "id_pixel"), ...)
{
    # Drop columns not needed
    x <- x %>% select(!all_of(exclude_variables))

    # Set random seed if seed is not NULL
    if( ! is.null(seed) ){ set.seed(seed) }

    # Output list
    results <- list()

    # Run the model for each value in n_centers
    k <- 1
    for(i in n_centers)
    {
        # Apply kmeans
        model <- kmeans(x = x, centers = i, nstart = nstart, ...)
        # Add index to model results
        model$calinski_harabasz_index <- index.G1(x, model$cluster)
        # Add non-scaled centers
        model$centers_not_scaled <- model$centers * attr(x, "rge")
        # Add results to output list.
        results[[k]] <- model

        k <- k +1
    }

    # Set names to output list
    names(results) <- as.character(n_centers)

    # Return
    return(results)
}

################################################################################
#' Plot results.
#'
#' This function is used to plot the number of clusters against the Calinski-Harabasz index
#'
#' @param kmeans_results a list of results obtained from the function kmeans_analysis
#' @importFrom ggplot2 ggplot geom_point geom_line xlab ylab aes
#' @export
#'
plot_results <- function(kmeans_results)
{
    # Get the number of clusters used
    clusters <- as.numeric(names(kmeans_results))
    # Get the Calinski-Harabasz index for each number of clusters as a vector
    c_a_index <- sapply(kmeans_results, function(x) x$calinski_harabasz_index)

    # Plot results
    ggp <- ggplot(data = data.frame(x = clusters, y = c_a_index), mapping = aes(x = x, y = y)) +
            geom_point() +
            geom_line() +
            xlab("Number of clusters") +
            ylab("Calinski-Harabasz pseudo F-statistic")
    print(ggp)
}

################################################################################
#' Extract results from the kmeans analysis
#'
#' This function is used to extract the best results from the kmeans analysis.
#' The best result is considered to be the result characterized by the highest
#' Calinski-Harabasz index.
#'
#' @param kmeans_results a list of results obtained from the function kmeans_analysis
#' @return A list with the best results (as defined above) of the kmeans analysis.
#' @export
#'
extract_results <- function(kmeans_results)
{
    # Get the names of the list (number of clusters used)
    clusters <- names(kmeans_results)
    # Get the Calinski-Harabasz index for each number of clusters
    c_a_index <- sapply(kmeans_results, function(x) x$calinski_harabasz_index)
    # Select the number of clusters that maximizes the index
    selected_number <- clusters[which.max(c_a_index)]
    # Get the results
    best_results <- kmeans_results[selected_number][[1]]
    # Return
    return(best_results)
}

################################################################################
#' Extract results from the kmeans analysis
#'
#' This function is used to extract the best results from the kmeans analysis.
#' The best result is considered to be the result characterized by the highest
#' Calinski-Harabasz index.
#'
#' @param x dataframe used in the kmeans analysis (prior to standardization)
#' @param nc_dataframe main dataframe
#' @param final_kmeans_results list of the final results obtained from the function extract_results
#' @param filename name of the .csv file to export
#' @param export_vars variables to export. A list of characters
#' @param join_by variables to join by. A vector of characters
#' @param unique_id a list of unique identifiers
#' @importFrom dplyr %>% select full_join distinct arrange all_of across
#' @export
#'
export_data <- function(x, nc_dataframe, final_kmeans_results, filename, export_vars = c("lon", "lat", "id_pixel", "gruppo"), join_by = c("id_pixel", "lon", "lat"), unique_id = c("lon", "lat", "id_pixel"))
{
    # Attach cluster
    x$gruppo <- final_kmeans_results$cluster
    # Select unique lon lat and idpixel from main dataframe
    new_nc <- nc_dataframe %>% select(all_of(unique_id)) %>% distinct()
    # Select only variables to export and join
    new_x <- x %>% select(all_of(export_vars)) %>% full_join(new_nc, by = join_by) %>% arrange(across(id_pixel))
    # Write data to csv
    write.csv(new_x, filename, row.names = FALSE)
    # Print
    print("Data written to file successfully!")
}
