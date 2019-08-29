#' Plot self-organizing map
#'
#' @param model An object of class "partition" created by the functions
#'   [cluster::pam()] or [stats::kmeans()].
#' @param data The original data frame used in the clustering.
#' @param colour_vector A vector of character or factors to colour the points
#'   by.
#' @param colour_label A label for the colour legend.
#' @param ... Other arguments to pass2 [factoextra::fviz_cluster()].
#'
#' @return A ggplot2 object.
#' @export
#'
#' @importFrom ggplot2 aes_string scale_colour_brewer ggplot geom_point
#' geom_polygon labs
#'
#' @examples
#' df <- haddock_mod %>%
#'   dplyr::select(ffmsy_median, bbmsy_median) %>%
#'   scale()#'

#' # Create the SOM Grid - you generally have to specify the size of the
#' # training grid prior to training the SOM. Hexagonal and Circular
#' # topologies are possible
#' ## dimensions chosen based on aiming for 5-10 assessments per class
#' som_grid <- somgrid(xdim = 3, ydim = 2, topo = "hexagonal")#'

#' # # Finally, train the SOM, options for the number of iterations,
#' # # the learning rates, and the neighbourhood are available
#' som_model <- som(df,
#'   grid = som_grid,
#'   rlen = 500,
#'   alpha = c(0.05, 0.01),
#'   keep.data = TRUE
#' )#'

#' plot_som(som_model,
#'   data = df, colour_vector = haddock_mod$fmodel,
#'   colour_label = "F model",
#'   nrow=2
#' )
plot_som <- function(model, data = NULL, colour_vector = NULL,
                          colour_label = "model", ...) {
  if (!class(model)[[1]] == c("kohonen")) {
    stop("Model must be of class kohonen.")
  }

  vals <- colnames(data)
  plot_info <- lapply(1:length(vals), function(x){
    out <-  plot_som_indiv(som_model,
                         data = df, val = vals[x], 
                         colour_vector = colour_vector,
                         colour_label = colour_label)
    return(out)
  })

  plot_grid(plotlist = plot_info, ...)
}
