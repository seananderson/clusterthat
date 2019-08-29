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
#' library(dplyr)
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
#'
#' # fake skill measure for now
#' df$cgcv <- runif(nrow(df),0,1)
#'
#' df %>%
#'   mutate(
#'     ens_cluster_wts  = ensemble_2stage_weights(cluster, cgcv)
#'   ) %>%
#'   mutate(
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
      group_by(cluster) %>%
      mutate(cluster_size = n(),
             cluster_skill = mean(weights)) %>%
      ungroup()

  # compute 2-stage weights
  .data <-
    .data %>%
    mutate(cluster_weight = cluster_skill / (cluster_size * nclusters)  )

  # return weights
  .data$cluster_weight
}

#' @rdname ensemble
#' @export
ensemble_2stage <- function(value, cluster, weights = NULL) {
  wts <- ensemble_2stage_weights(cluster, weights)
  ensemble_simple(value, wts)
}
