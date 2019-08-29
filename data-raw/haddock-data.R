
library(FLCore)
library(FLa4a)
load("data-raw/fits.rdata")

haddock_ts <- clusterthat::fits2data_ts(fits)
haddock_mod <- clusterthat::fits2data_mod(fits, fo, ffmsy, bbmsy)

usethis::use_data(haddock_mod, overwrite = TRUE)
usethis::use_data(haddock_ts, overwrite = TRUE)
