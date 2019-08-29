haddock_mod <- read.csv("data-raw/haddock_clustering_mod.csv", stringsAsFactors = FALSE)
haddock_ts <- read.csv("data-raw/haddock_clustering_ts.csv", stringsAsFactors = FALSE)

usethis::use_data(haddock_mod, overwrite = TRUE)
usethis::use_data(haddock_ts, overwrite = TRUE)
