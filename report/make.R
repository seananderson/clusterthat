rmarkdown::render("dfa-clustering-template.Rmd",
  params = list(
    stock = "Haddock",
    timeseries_data = clusterthat::haddock_ts,
    model_data = clusterthat::haddock_mod
  ),
  output_file = "haddock-dfa.html"
)

rmarkdown::render("dfa-clustering-template.Rmd",
  params = list(
    stock = "Cod",
    timeseries_data = clusterthat::cod_ts,
    model_data = clusterthat::cod_mod
  ),
  output_file = "cod-dfa.html"
)

# rmarkdown::render("dfa-clustering-template.Rmd",
#   params = list(
#     stock = "Haddock",
#     timeseries_data = clusterthat::,
#     model_data = clusterthat::haddock_mod
#   ),
#   output_file = "haddock-dfa.html"
# )

# --------------------------------------------------------------------------

rmarkdown::render("simple-example-template.Rmd",
  params = list(
    stock = "Haddock",
    timeseries_data = clusterthat::haddock_ts,
    model_data = clusterthat::haddock_mod
  ),
  output_file = "haddock-simple.html"
)

rmarkdown::render("simple-example-template.Rmd",
  params = list(
    stock = "Cod",
    timeseries_data = clusterthat::cod_ts,
    model_data = clusterthat::cod_mod
  ),
  output_file = "cod-simple.html"
)
