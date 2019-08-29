
# to create the fits.data file in this folder, from
# the outputs created by the groups scripts, use:
#
# fits <- lapply(fits, "[", c("stk", "fit", "model", "rp"))
# save(fits, fo, ffmsy, bbmsy, wts, file = "fits.rdata")
#
# this takes it from ~ 1 GB to ~ 5 MB

library(FLCore)
library(FLa4a)
load("data-raw/fits.rdata")

haddock_ts <- clusterthat::fits2data_ts(fits2)
haddock_mod <- clusterthat::fits2data_mod(fits2, fo, ffmsy, bbmsy)

usethis::use_data(haddock_mod, overwrite = TRUE)
usethis::use_data(haddock_ts, overwrite = TRUE)
