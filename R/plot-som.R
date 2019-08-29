#' Plot self-organizing map
#'
#' @param model An object as output from [kohonen::som()].
#' @param data The original data frame used in the clustering.
#' @param val name of value to plot that matches the variable in the data column
#' @param colour_vector A vector of character or factors to colour the points
#'   by.
#' @param colour_label A label for the colour legend.
#' @param ... Other arguments to pass2 [factoextra::fviz_cluster()].
#'
#' @return A ggplot2 object.
#' @export
#' @rdname plot_som
#'
#' @importFrom dplyr full_join
#' @importFrom ggplot2 geom_hex
#'
#' @examples
#' df <- haddock_mod %>%
#'   dplyr::select(fo_median, ffmsy_median, bbmsy_median) %>%
#'   scale()
#'
#' # Create the SOM Grid - you generally have to specify the size of the
#' # training grid prior to training the SOM. Hexagonal and Circular
#' # topologies are possible
#' # dimensions chosen based on aiming for 5-10 assessments per class
#' som_grid <- kohonen::somgrid(xdim = 3, ydim = 2, topo = "hexagonal")
#'
#' # Finally, train the SOM, options for the number of iterations,
#' # the learning rates, and the neighbourhood are available
#' som_model <- kohonen::som(df,
#'   grid = som_grid,
#'   rlen = 500,
#'   alpha = c(0.05, 0.01),
#'   keep.data = TRUE
#' )
#'
#' plot_som_individual(som_model,
#'   data = df, val = "ffmsy_median",
#'   colour_vector = haddock_mod$fmodel,
#'   colour_label = "F model"
#' )
#'
#' plot_som(som_model,
#'   data = df, colour_vector = haddock_mod$fmodel,
#'   colour_label = "F model",
#'   nrow = 2
#' )
plot_som_individual <- function(model, data, val, colour_vector = NULL,
                                colour_label = "model") {
  ## data frame with data and applied class from SOM
  class <- data.frame(
    "Model" = seq(1, length(model$unit.classif)),
    "Class" = model$unit.classif
  )

  ## number of models in each class
  count <- sapply(1:nrow(model$grid$pts), function(x) length(which(class$Class == x)))

  ## grid for plotting hexagons
  grid <- data.frame(model$grid$pts, Class = 1:nrow(model$grid$pts), Count = count)

  ## value unscaled
  val_unscaled <- sapply(1:ncol(data), function(x)
    stats::aggregate(as.numeric(data[, x]),
      by = list(model$unit.classif),
      FUN = mean, simplify = TRUE
    )[, 2])
  val_df <- data.frame(val_unscaled)
  colnames(val_df) <- paste0(colnames(data))
  val_df <- val_df %>% mutate(Class = seq(1, nrow(val_df)))

  grid <- full_join(grid, val_df, by = "Class")

  ## data frame with class and grid info
  input <- full_join(class, grid, by = "Class")

  p <- ggplot(input) +
    geom_hex(aes_string(x = "x", y = "y", fill = val),
      color = "black", stat = "identity"
    ) +
    ggplot2::coord_equal(xlim = c(
      min(input$x) - 0.5,
      max(input$x) + 0.5
    ), ylim = c(min(input$y) - 0.5, max(input$y) + 0.5)) +
    ggplot2::theme_void() +
    ggrepel::geom_label_repel(aes_string(
      x = "x", y = "y",
      label = "Model", color = "colour_vector"
    ), seed = 1) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(colour = colour_label) +
    ggplot2::scale_color_brewer(palette = "Set2")

  p
}

#' @param ... Other arguments to past to [cowplot::plot_grid()].
#' @export
#' @rdname plot_som
plot_som <- function(model, data, colour_vector = NULL,
                     colour_label = "model", ...) {
  if (!class(model)[[1]] == c("kohonen")) {
    stop("Model must be of class kohonen.")
  }

  vals <- colnames(data)
  plot_info <- lapply(seq_len(length(vals)), function(x) {
    out <- plot_som_individual(model,
      data = data, val = vals[x],
      colour_vector = colour_vector,
      colour_label = colour_label
    )
    out
  })

  cowplot::plot_grid(plotlist = plot_info, ...)
}
