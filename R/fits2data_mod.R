#' Get model level data
#'
#' Create a time series dataset from a list of FLStocks and FL fit objects
#'
#' @param fits a list containing the elements
#' @param fo a matrix containing simulated values for each model
#' @param ffmsy a matrix containing simulated values for each model
#' @param bbmsy a matrix containing simulated values for each model
#' @param wts a matrix containing weight values for each model
#'
#' @return A data.frame object
#' @export
#' @importFrom stats median sd
fits2data_mod <- function(fits, fo, ffmsy, bbmsy, wts) {
  df <-
    do.call(rbind,
            lapply(fits, function(fit) {
              dat <- data.frame(Bmsy = NA, Fmsy = NA)
              if (!is.null(fit$rp)) {
                dat$Bmsy <- c(fit$rp["msy", "ssb"])
                dat$Fmsy <- c(fit$rp["msy", "harvest"])
              }
              dat$model_id = fit$model$model_id
              dat
            })
    )
  rownames(df) <- NULL


  cv <- function(x) stats::sd(x) / mean(x)

  df <-
    data.frame(model_id = 1:ncol(fo),
               fo_median    = apply(fo, 2, stats::median),
               fo_cv        = apply(fo, 2, cv),
               ffmsy_median = apply(ffmsy, 2, stats::median),
               ffmsy_cv     = apply(ffmsy, 2, cv),
               bbmsy_median = apply(bbmsy, 2, stats::median),
               bbmsy_cv     = apply(bbmsy, 2, cv)) %>%
    dplyr::left_join(df) %>%
    dplyr::left_join(wts)

  names(df) <- tolower(names(df))
  df <- dplyr::filter(df, !is.na(bbmsy_median))

  df <- tibble::as_tibble(df)

  df
}
