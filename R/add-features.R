#' Add some possible clustering features to FLR output
#'
#' @param data_mod Model output data.
#' @param data_ts Model output time series data.
#' @param n_years_slope Number of years for the slope calculation on F and SSB.
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr group_by mutate transmute
#' @examples
#' add_features_flr(haddock_mod, haddock_ts, n_years_slope = 5)
add_features_flr <- function(data_mod, data_ts, n_years_slope = 5) {
  data_ts <- dplyr::filter(data_ts, !is.na(ssb), !is.na(fbar))
  years_of_interest <- rev(sort(unique(data_ts$year)))[seq_len(n_years_slope)]

  slopes <- data_ts %>%
    reshape2::melt(id.vars = c("year", "model_id")) %>%
    group_by(.data$variable, .data$model_id) %>%
    tidyr::nest() %>%
    mutate(model = purrr::map(data, ts_model,
      years_of_interest = years_of_interest
    )) %>%
    mutate(slope = purrr::map(model, ~ coef(.x)[[2]])) %>%
    tidyr::unnest(slope, .drop = TRUE)

  slopes <- reshape2::dcast(slopes, model_id ~ variable, value.var = "slope") %>%
    dplyr::rename(fbar_slope = .data$fbar, ssb_slope = .data$ssb)

  d <- dplyr::left_join(data_mod, slopes, by = "model_id")
  d <- dplyr::filter(d, !is.na(ssb_slope))

  dplyr::transmute(d,
    log_fo_median = log(fo_median), log_fo_cv = log(fo_cv),
    fbar_slope = fbar_slope, ssb_slope = ssb_slope,
    log_ffmsy_median = log(ffmsy_median),
    log_ffmsy_cv = log(ffmsy_cv),
    log_bbmsy_median = log(bbmsy_median),
    log_bbmsy_cv = log(bbmsy_cv)
  ) %>% tibble::as_tibble()
}

ts_model <- function(df, years_of_interest) {
  df <- dplyr::filter(df, year %in% years_of_interest)
  stats::lm(log(value) ~ year, data = df)
}


if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("bbmsy_cv", "bbmsy_median", "data", "fbar", "fbar_slope", "ffmsy_cv",
      "ffmsy_median",
      "fo_cv", "fo_median", "model", "slope", "ssb",
      "ssb_slope", "year")
  )
}
