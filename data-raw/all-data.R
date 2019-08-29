
# to create the fits.data file in this folder, from
# the outputs created by the groups scripts, use:
#
# fits <- lapply(fits, "[", c("stk", "fit", "model", "rp"))
# save(fits, fo, ffmsy, bbmsy, wts, file = "fits.rdata")
#
# this takes it from ~ 1 GB to ~ 5 MB

library(FLCore)
library(FLa4a)
load("data-raw/fits_haddock.rdata")

haddock_ts <- clusterthat::fits2data_ts(fits2)
haddock_mod <- clusterthat::fits2data_mod(fits2, fo, ffmsy, bbmsy, wts)

usethis::use_data(haddock_mod, overwrite = TRUE)
usethis::use_data(haddock_ts, overwrite = TRUE)


# cod data

load("data-raw/fits_cod.rdata")

cod_ts <- clusterthat::fits2data_ts(fits2)
cod_mod <- clusterthat::fits2data_mod(fits2, fo, ffmsy, bbmsy, wts)

usethis::use_data(cod_ts, overwrite = TRUE)
usethis::use_data(cod_mod, overwrite = TRUE)

# g3 sims

load("data-raw/fits_g3.rdata")

simg3_ts <- clusterthat::fits2data_ts(fits2)
simg3_mod <- clusterthat::fits2data_mod(fits2, fo, ffmsy, bbmsy, wts)

usethis::use_data(simg3_ts, overwrite = TRUE)
usethis::use_data(simg3_mod, overwrite = TRUE)

# s3 sims

load("data-raw/fits_s3.rdata")

sims3_ts <- clusterthat::fits2data_ts(fits2)
sims3_mod <- clusterthat::fits2data_mod(fits2, fo, ffmsy, bbmsy, wts)

usethis::use_data(sims3_ts, overwrite = TRUE)
usethis::use_data(sims3_mod, overwrite = TRUE)
