#' Create time series dataset
#'
#' Create a time series dataset from a list of FLStocks and FL fit objects
#'
#' @param fits a list containing the elements
#'
#' @return A data.frame object
#' @export
fits2data_ts <- function(fits) {
  # get time series of f and ssb
  df <-
    do.call(
      rbind,
        lapply(fits, function(fit) {
          x <- fit$stk + fit$fit
          rbind(cbind(what = "ssb", FLCore::as.data.frame(ssb(x))),
                cbind(what = "fbar", FLCore::as.data.frame(fbar(x)))) %>%
            select(year, data, what) %>%
            mutate(model_id = fit$model$model_id)
        })
    )
  rownames(df) <- NULL

  names(df) <- tolower(names(df))
  df <- dplyr::filter(df, !is.na(data))

  df <-
    df %>%
    tibble::as_tibble() %>%
    reshape2::dcast(year + model_id ~ what, value.var = "data") %>%
    tibble::as_tibble() %>%
    dplyr::arrange(model_id, year)

  df
}
