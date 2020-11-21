################################################################################
#' Fit a random forest model
#'
#' @param formula A formula for the random forest such as y~x.
#' @param data Data to be used.
#' @param ntree Number of trees to grow. This should not be set to too small.
#' @param na.action A function to specify the action to be taken if NAs are found
#' @param importance Should importance of predictors be assessed? Boolean
#' @param seed Random seed for reproducibility purposes.
#' @param ... Other arguments passed to the randomForest::randomForest function
#' @importFrom randomForest randomForest
#' @export
#'
fit_random_forest <- function(formula, data, ntree = 1000, na.action = na.omit, importance = TRUE, seed = NULL, ...)
{
    # Set random seed
    if( !is.null(seed) ){ set.seed(seed) }

    # Fit a random forest model
    model_fit <- randomForest(formula = formula,
                              data = data,
                              ntree = ntree,
                              na.action = na.action,
                              importance = importance,
                              ...)
    # Return
    return(model_fit)
}

################################################################################
#' Get variable importance
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @importFrom randomForest importance
#' @export
#'
get_variable_importance <- function(rf_model)
{
    # Get variable importance
    var_imp <- importance(rf_model)
    # Return
    return(var_imp)
}

################################################################################
#' Plot variable importance
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param title plot title
#' @param sort sort variables in order of variable importance? Boolean
#' @param ... other arguments
#' @importFrom randomForest varImpPlot
#' @export
#'
variable_importance_plot <- function(rf_model, title = "Variable importance plot", sort = TRUE, ...)
{
    # Plot variable importance
    varImpPlot(rf_model, main = title, sort = sort, ...)
}

################################################################################
#' Plot model error vs number of trees
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param title plot title
#' @export
#'
plot_error_vs_trees <- function(rf_model, title)
{
    # Model error vs number of trees
    plot(rf_model, main = title)
}

################################################################################
#' Partial dependence plot
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param data data used to fit the random forest model
#' @param return_plot should plots be showed? Boolean. FALSE by default.
#' @param cols number of columns to use when plotting the dependence plots. Defaults to 2.
#' @param ylabel label of the y axis. Character. Defaults to "y".
#' @param verbose be verbose? Boolean.
#' @importFrom randomForest importance
#' @importFrom ggplot2 ggplot geom_line geom_label xlab ggtitle ylab
#' @export
#'
#' @return a list with x and y values for the partial dependence graph for each variable. If show_plots
#' is TRUE, NULL is returned.
#'
partial_dependence_plot <- function(rf_model, data, return_plot = FALSE, cols = 2, ylabel = "y", verbose = FALSE)
{
    # Convert data to a dataframe
    data <- as.data.frame(data)
    # Calculate variable importance
    imp <- importance(rf_model)
    # Order v. names by importance
    impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]

    # Outputlist
    data_out <- list()

    # Calculate partial dependence data
    for (i in seq_along(impvar))
    {
        # Current variable
        var <- impvar[i]

        if(verbose) { print(paste("Calculating variable dependence on", var, sep = " ")) }

        # Calculate partial dependence
        partial_dep_data <- ppdplot(rf_model,
                    pred.data = data,
                    x.var = var,
                    #n.pt = 10,
                    plot = F)

        # Ouput data
        data_out[[var]] <- partial_dep_data
    }

    # If return_plot == TRUE return a ggplot object, otherwise simply return partial dependence data.
    if(return_plot)
    {
        # List of ggplots
        plts <- list()
        # Plot all partial dependence plots
        for(i in seq_along(impvar))
        {
            var <- impvar[i]
            df <- as.data.frame(data_out[[i]])
            plotdf <- ggplot(df, aes(x = x, y = y)) +
                geom_line() +
                ggtitle(paste("Partial dependence on", var, sep = " ")) +
                xlab(var) +
                ylab(ylabel)

            # Store plot in a list
            plts[[i]] <- plotdf
        }
        # Generate multiplot grid
        multiplot(plots = plts, cols = cols)
        return(invisible(NULL))
    }

    # Return
    return(data_out)
}

################################################################################
#' Plot a single partial dependence plot
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param data data used to fit the random forest model
#' @param variable variable to use. A character.
#' @param ylabel label of the y axis. Character. Defaults to "y".
#' @param return_plot Boolean. If TRUE an object of class ggplot is returned, if FALSE
#' the data about partial dependence is calculated and returned. FALSE by default.
#' @importFrom ggplot2 ggplot geom_line geom_label xlab ggtitle ylab
#' @export
#'
#' @return Data on partial dependence for the selected variable or a ggplot object depending
#' on the argument return_plot.
#'
single_partial_dependence_plot <- function(rf_model, data, variable, return_plot = FALSE, ylabel = "y")
{
    # Calculate partial dependence data
    partial_dep_data <- ppdplot(rf_model,
                                pred.data = as.data.frame(data),
                                x.var = variable,
                               # n.pt = 10,
                                plot = F)

    df <- as.data.frame(partial_dep_data)

    # Generate plot if return_plot == TRUE. Otherwise, simply return partial dependence data
    if( return_plot )
    {
        out <- ggplot(df, aes(x = x, y = y)) +
            geom_line() +
            ggtitle(paste("Partial dependence on", variable, sep = " ")) +
            xlab(variable) +
            ylab(ylabel)
    }else
    {
        out <- df
    }

    # Return plot
    return(out)
}

################################################################################
#' Plot predictive map
#'
#' @param rf_model random forest model obtained from the function fit_random_forest.
#' @param data data used to fit the random forest model
#' @param facet_plot Should the plot be categorized? If TRUE, then the argument "facet_by" must be set. If FALSE,
#' an average prediction is computed and plotted. Boolean
#' @param facet_by variable to use in facet plots. Character.
#' @param lon_lat_names names of latitude and longitude in the dataset. A list of characters. Defaults to list("lon", "lat")
#' @importFrom ggplot2 ggplot geom_tile stat_contour facet_wrap aes_string
#' @importFrom dplyr %>% select group_by mutate summarise all_of across
#' @importFrom lazyeval interp
#' @export
#'
predictive_map <- function(rf_model, data, facet_plot = FALSE, facet_by = "year", lon_lat_names = list("lon", "lat"))
{
    # Make predictions
    z <- predict(rf_model, data)

    # Bind predicted data
    data <- cbind(data, pred = z)

    # Define list for dplyr functions
    lon_lat_names[[3]] <- "pred"
    # Variables to select
    var_select <- lon_lat_names
    # Lon and lat names
    ll_names <- lon_lat_names[1:2]
    # facet_wrap formula
    facet_wrap_formula <- interp( ~x, x = as.name(facet_by))

    if(!facet_plot)
    {
        # Select data and group
        data <- data %>%
            select(all_of(var_select)) %>%
            group_by(across(ll_names)) %>%
            summarise(pred = mean(pred, na.rm = TRUE)) %>%
            mutate(year = "Average")
    }

    # Plot
    lon_name <- lon_lat_names[[1]]
    lat_name <- lon_lat_names[[2]]
    ggp <- ggplot(data, aes_string(x = lon_name, y = lat_name, z = "pred")) +
        geom_tile(aes(fill = pred)) +
        stat_contour() +
        facet_wrap(facet_wrap_formula)

    # Return ggplot object
    return(ggp)
}

################################################################################
#' Plot partial dependence plots. A revised version of the official function.
#' The function had to be revised due to the use of NSE on the x.var argument.
#'
#' @param x altro
#' @param pred.data altro
#' @param x.var altro
#' @param which.class altro
#' @param w altro
#' @param plot altro
#' @param add altro
#' @param n.pt altro
#' @param rug altro
#' @param xlab altro
#' @param ylab altro
#' @param main altro
#' @param ... altro
#'
#' @export
#'
ppdplot <- function (x, pred.data, x.var, which.class, w, plot = TRUE,
                 add = FALSE, n.pt = min(length(unique(pred.data[, xname])),
                                         51), rug = TRUE, xlab = deparse(substitute(x.var)),
                 ylab = "", main = paste("Partial Dependence on", deparse(substitute(x.var))),
                 ...)
{
    classRF <- x$type != "regression"
    if (is.null(x$forest))
        stop("The randomForest object must contain the forest.\n")
    xname <- x.var
    xv <- pred.data[, xname]
    n <- nrow(pred.data)
    if (missing(w))
        w <- rep(1, n)
    if (classRF) {
        if (missing(which.class)) {
            focus <- 1
        }
        else {
            focus <- charmatch(which.class, colnames(x$votes))
            if (is.na(focus))
                stop(which.class, "is not one of the class labels.")
        }
    }
    if (is.factor(xv) && !is.ordered(xv)) {
        x.pt <- levels(xv)
        y.pt <- numeric(length(x.pt))
        for (i in seq(along = x.pt)) {
            x.data <- pred.data
            x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
            if (classRF) {
                pr <- predict(x, x.data, type = "prob")
                y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] >
                                                        0, pr[, focus], .Machine$double.eps)) - rowMeans(log(ifelse(pr >
                                                                                                                        0, pr, .Machine$double.eps))), w, na.rm = TRUE)
            }
            else y.pt[i] <- weighted.mean(predict(x, x.data),
                                          w, na.rm = TRUE)
        }
        if (add) {
            points(1:length(x.pt), y.pt, type = "h", lwd = 2,
                   ...)
        }
        else {
            if (plot)
                barplot(y.pt, width = rep(1, length(y.pt)),
                        col = "blue", xlab = xlab, ylab = ylab, main = main,
                        names.arg = x.pt, ...)
        }
    }
    else {
        if (is.ordered(xv))
            xv <- as.numeric(xv)
        x.pt <- seq(min(xv, na.rm = T), max(xv, na.rm = T), length = n.pt)
        y.pt <- numeric(length(x.pt))
        for (i in seq(along = x.pt)) {
            x.data <- pred.data
            x.data[, xname] <- rep(x.pt[i], n)
            if (classRF) {
                pr <- predict(x, x.data, type = "prob")
                y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] ==
                                                        0, .Machine$double.eps, pr[, focus])) - rowMeans(log(ifelse(pr ==
                                                                                                                        0, .Machine$double.eps, pr))), w, na.rm = TRUE)
            }
            else {
                y.pt[i] <- weighted.mean(predict(x, x.data),
                                         w, na.rm = TRUE)
            }
        }
        if (add) {
            lines(x.pt, y.pt, ...)
        }
        else {
            if (plot)
                plot(x.pt, y.pt, type = "l", xlab = xlab, ylab = ylab,
                     main = main, ...)
        }
        if (rug && plot) {
            if (n.pt > 10) {
                rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
            }
            else {
                rug(unique(xv, side = 1))
            }
        }
    }
    invisible(list(x = x.pt, y = y.pt))
}

################################################################################
#' Fit multiple plots in a single plot
#'
#'
#' @param plots a list of ggplot objects
#' @param cols number of columns to display the plots
#' @param layout layout
#' @importFrom grid grid.newpage pushViewport viewport grid.layout
#'
#' @export
#'
multiplot <- function(plots = NULL, cols=1, layout = NULL)
{
    numPlots = length(plots)
    # If layout is NULL, then use 'cols' to determine layout
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))

    if (numPlots==1)
    {
        print(plots[[1]])
    }else
    {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots)
        {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
