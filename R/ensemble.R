#' Ensemble multiple input values
#'
#' Creates an ensemble of multiple input values based on provided
#' weightss
#'
#' @param value a vector holding the values to ensemble over
#' @param cluster a vector of cluster ids defining the cluster
#'   membership of the value
#' @param weights margin parameters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return vector of numeric values
#'
#' @details
#'
#' not yet!
#'
#'
#' @examples
#'
#' # get data
#' data(haddock_mod)
#' df <-
#'   haddock_mod %>%
#'   dplyr::select(fo_median, ffmsy_median, bbmsy_median) %>%
#'   scale()
#'
#' # Illustrate an example with 2 clusters:
#' m <- kmeans(df, centers = 2L)
#'
#' plot_clusters(m,
#'   data = df, colour_vector = haddock_mod$fmodel,
#'   colour_label = "F model"
#' )
#'
#' # calculate weighting
#' weights <-
#'   haddock_mod %>%
#'   dplyr::select(model_id, ffmsy_median, cgcv)
#' weights$cluster <- m$cluster
#'
#'
#' # do a simple ensemble - no weighting
#' weights %>%
#'   dplyr::mutate(
#'     ens_simple = ensemble_simple(ffmsy_median)
#'   )
#'
#' # do a simple cluster ensemble - no weighting
#' weights %>%
#'   dplyr::group_by(cluster) %>%
#'   dplyr::mutate(
#'     ens_cluster = ensemble_simple(ffmsy_median)
#'   )
#'
#' # use skill level from cgcv
#' weights %>%
#'   dplyr::mutate(
#'     ens_cluster_wts  = ensemble_2stage_weights(cluster, cgcv)
#'   ) %>%
#'   dplyr::mutate(
#'     ens_cluster = ensemble_simple(ffmsy_median, ens_cluster_wts)
#'   )
#'
#' @rdname ensemble
#' @name ensembleMethods
NULL

#' @rdname ensemble
#' @export
#' @importFrom stats weighted.mean
ensemble_simple <- function(value, weights = NULL) {
  # get vlues to cluster over

  # weights if provided
  if (is.null(weights)) {
    weights <- rep(1, length(value))
  }

  # do clustering
  weighted.mean(value, weights)
}


#' @rdname ensemble
#' @export
#' @importFrom dplyr group_by mutate ungroup
ensemble_2stage_weights <- function(cluster, weights = NULL) {

  # create a df
  .data <- data.frame(cluster = factor(cluster))
  .data$weights <- if(is.null(weights)) 1 else weights

  # total number of clusters
  .data$nclusters <- length(unique(cluster))

  # get all the bits of info we need
  .data <-
    .data %>%
      dplyr::group_by(cluster) %>%
      dplyr::mutate(cluster_size = dplyr::n(),
                    cluster_skill = mean(weights)) %>%
      dplyr::ungroup()

  # compute 2-stage weights
  .data <-
    .data %>%
    dplyr::mutate(cluster_weight = cluster_skill / (cluster_size * nclusters)  )

  # return weights
  .data$cluster_weight
}

#' @rdname ensemble
#' @export
ensemble_2stage <- function(value, cluster, weights = NULL) {
  wts <- ensemble_2stage_weights(cluster, weights)
  ensemble_simple(value, wts)
}
