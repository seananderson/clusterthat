#' Plot self-organizing map
#'
#' @param model An object of class "partition" created by the functions
#'   [cluster::pam()] or [stats::kmeans()].
#' @param data The original data frame used in the clustering.
#' @param val name of value to plot that matches the variable in the data column
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
#'   dplyr::select(fo_median, ffmsy_median, bbmsy_median) %>%
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

#' plot_som_indiv(som_model,
#'   data = df, val = 'ffmsy_median', 
#'   colour_vector = haddock_mod$fmodel,
#'   colour_label = "F model"
#' )
plot_som_indiv <- function(model, data = NULL, val, colour_vector = NULL,
                          colour_label = "model", ...) {
  ## data frame with data and applied class from SOM
  class <- data.frame("Model"=1:length(model$unit.classif), "Class"=model$unit.classif)

  ## number of models in each class
  count <- data.frame(table(class$Class))[,"Freq"]

  ## grid for plotting hexagons
  grid <- data.frame(model$grid$pts, Class = 1:nrow(model$grid$pts), Count = count)

  ## value unscaled
  val_unscaled <- sapply(1:ncol(data), function(x) aggregate(as.numeric(data[,x]), by = list(model$unit.classif), FUN = mean, simplify = TRUE)[,2])
  val_df <- data.frame(val_unscaled)
  colnames(val_df) <- paste0(colnames(data))
  val_df <- val_df %>% mutate(Class = 1:nrow(val_df))

  grid <- full_join(grid, val_df)

  ## data frame with class and grid info
  input <- full_join(class, grid)

  p <- ggplot(input) +
	    geom_hex(aes_string(x="x", y="y", fill=val ), color = "black", stat="identity") +
	    coord_equal(xlim=c(min(input$x)-0.5,max(input$x)+0.5), ylim=c(min(input$y)-0.5,max(input$y)+0.5)) +
	    theme_void() +
	    geom_label_repel(aes(x=x, y=y, label=Model, color = colour_vector), seed=1) +
      scale_fill_viridis_c() +
      labs(colour = colour_label) +
	    scale_color_brewer(palette = "Set2") 

  p
}
