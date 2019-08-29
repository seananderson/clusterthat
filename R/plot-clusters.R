#' Plot clusters
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
# @examples
# m <- kmeans(haddock_mod, 2)
# plot_clusters(m, data = df, colour_vector = .d$fmodel,
# colour_label = "F model")

plot_clusters <- function(model, data = NULL, colour_vector = NULL,
  colour_label = "model", ...) {
  g <- factoextra::fviz_cluster(model,
    data = data,
    ellipse.type = "convex",
    ...
  )
  gdat <- ggplot2::ggplot_build(g)$data

  if (!is.null(colour_vector)) {
    gdat[[1]] <- data.frame(gdat[[1]], colour_vector = colour_vector)
  } else {
    gdat[[1]] <- data.frame(gdat[[1]], colour_vector = 1)
  }

  gg <- ggplot(gdat[[1]], aes_string("x", "y")) +
    geom_point(aes_string(colour = "colour_vector"), size = 2.5) +
    geom_polygon(
      data = gdat[[2]], aes_string(x = "x", y = "y", group = "group"),
      fill = NA, colour = "grey50"
    ) +
    ggplot2::theme_minimal() +
    ggrepel::geom_text_repel(data = gdat[[4]], aes_string("x", "y", label = "label")) +
    labs(x = g$labels$x, y = g$labels$y, colour = colour_label) +
    scale_colour_brewer(palette = "Set2")

  gg
}
