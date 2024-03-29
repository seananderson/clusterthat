% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ensemble.R
\name{ensembleMethods}
\alias{ensembleMethods}
\alias{ensemble_simple}
\alias{ensemble_2stage_weights}
\alias{ensemble_2stage}
\title{Ensemble multiple input values}
\usage{
ensemble_simple(value, weights = NULL)

ensemble_2stage_weights(cluster, weights = NULL)

ensemble_2stage(value, cluster, weights = NULL)
}
\arguments{
\item{value}{a vector holding the values to ensemble over}

\item{weights}{margin parameters; vector of length 4 (see \code{\link[graphics]{par}})}

\item{cluster}{a vector of cluster ids defining the cluster
membership of the value}
}
\value{
vector of numeric values
}
\description{
Creates an ensemble of multiple input values based on provided
weightss
}
\details{
not yet!
}
\examples{

# get data
data(haddock_mod)
df <-
  haddock_mod \%>\%
  dplyr::select(fo_median, ffmsy_median, bbmsy_median) \%>\%
  scale()

# Illustrate an example with 2 clusters:
m <- kmeans(df, centers = 2L)

plot_clusters(m,
  data = df, colour_vector = haddock_mod$fmodel,
  colour_label = "F model"
)

# calculate weighting
weights <-
  haddock_mod \%>\%
  dplyr::select(model_id, ffmsy_median, cgcv)
weights$cluster <- m$cluster


# do a simple ensemble - no weighting
weights \%>\%
  dplyr::mutate(
    ens_simple = ensemble_simple(ffmsy_median)
  )

# do a simple cluster ensemble - no weighting
weights \%>\%
  dplyr::group_by(cluster) \%>\%
  dplyr::mutate(
    ens_cluster = ensemble_simple(ffmsy_median)
  )

# use skill level from cgcv
weights \%>\%
  dplyr::mutate(
    ens_cluster_wts = ensemble_2stage_weights(cluster, cgcv)
  ) \%>\%
  dplyr::mutate(
    ens_cluster = ensemble_simple(ffmsy_median, ens_cluster_wts)
  )
}
