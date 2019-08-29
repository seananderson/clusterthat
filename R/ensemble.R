#' Ensemble multiple input values
#'
#' Creates an ensemble of multiple input values based on provided
#' weightss
#'
#' @param .data a code{data.frame} with columns holding the data to
#'   ensemble over, cluster ids, and optional weights
#' @param method one of: simple, cluster, 2stage (see details)
#' @param cluster_id a vector of cluster ids defining the cluster
#'   membership of the value
#' @param weights margin parameters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return vector of numeric values
#'
#' @details
#'
#' not yet!
#'
#' @examples
#'
#' # get data
#' data(haddock_mod)
#' df <-
#'   haddock_mod %>%
#'   select(fo_median, ffmsy_median, bbmsy_median)
#'
#' # perform simple clustering on a few quantities
#' m <-
#'   df %>% kmeans(centers = 2)
#'
#' # a plot
#' factoextra::fviz_cluster(m, data = df, axis = c(1,2))
#'
#' # calculate weighting?
#' df$cluster <- m$cluster
#'
#' # do a simple ensemble - no weighting
#' df %>%
#'   mutate(
#'     ens_simple = ensemble_simple(ffmsy_median)
#'   )
#'
#' # do a simple cluster ensemble - no weighting
#' df %>%
#'   group_by(cluster) %>%
#'   mutate(
#'     ens_cluster = ensemble_simple(ffmsy_median)
#'   )
#'
#' @export
ensemble_simple <- function(value, weights = NULL) {
  # get vlues to cluster over

  # weights if provided
  if (is.null(weights)) {
    weights <- rep(1, length(value))
  }

  # do clustering
  weighted.mean(value, weights)
}

ensemble_cluster <- function(value, cluster_id, weights = NULL) {

  stop("not working yet")
  .data <- data.frame(value = value, cluster_id = cluster_id)
  .data$weights <- weights

  .data %>%
    group_by_(cluster_id) %>%
    mutate(
      ensemble = ensemble_simple(value, weights)
    )

}

ensemble_2stage <- function(.data, value, cluster_id, weights) {

}

