haddock_mod <- read.csv("data-raw/haddock_clustering_mod.csv", stringsAsFactors = FALSE)
haddock_ts <- read.csv("data-raw/haddock_clustering_ts.csv", stringsAsFactors = FALSE)

haddock_mod <- dplyr::select(haddock_mod, -X)
names(haddock_mod) <- tolower(names(haddock_mod))
haddock_mod <- dplyr::filter(haddock_mod, !is.na(bbmsy_median))

haddock_ts <- dplyr::select(haddock_ts, -X)
names(haddock_ts) <- tolower(names(haddock_ts))
haddock_ts <- dplyr::filter(haddock_ts, !is.na(data))

haddock_ts <- tibble::as_tibble(haddock_ts)
haddock_mod <- tibble::as_tibble(haddock_mod)

usethis::use_data(haddock_mod, overwrite = TRUE)
usethis::use_data(haddock_ts, overwrite = TRUE)
