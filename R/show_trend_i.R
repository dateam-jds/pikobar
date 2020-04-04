#' Show interactive trend plot
#'
#' @param trend A COVID-19 trend object.
#' @importFrom highcharter hchart hcaes hc_add_series hc_xAxis hc_yAxis hc_title hc_subtitle hc_tooltip hc_add_theme hc_theme_smpl
#' @importFrom glue glue
#'
#' @return A highchart object.
#' @export
#'
#' @examples
#' # Fetch data
#' daily <- fetch_data("Jawa Barat")
#'
#' # Inspect incidence
#' covid_incidence <-
#'   inspect_incidence(daily, "positif")
#' show_incidence_s(covid_incidence)
#'
#' # Inspect daily trend
#' covid_trend <-
#'   inspect_trend(covid_incidence)
#' show_trend_i(covid_trend)
show_trend_i <- function(trend) {
  if (!inherits(trend, "covid19_trend")) {
    stop("Please supply a valid COVID-19 trend.", call. = FALSE)
  }

  which <- attr(trend, "which")
  location <- attr(trend, "location")

  last_updated <- trend$last_updated %>%
    as.POSIXct() %>%
    format(format = "%e %b %Y %H:%M", tz = "Asia/Jakarta", usetz = TRUE)

  res <-
    trend[["r_estimates"]] %>%
    hchart(
      "arearange",
      hcaes(date, low = lower_bound, high = upper_bound),
      name = "95% CI",
      color = "#43CD80",
      fillOpacity = 0.3
    ) %>%
    hc_add_series(
      trend[["r_estimates"]],
      "line",
      hcaes(date, r),
      name = "RERATA",
      color = "#43CD80"
    ) %>%
    hc_xAxis(title = list(text = NULL)) %>%
    hc_yAxis(
      title = list(text = "INSTANTANEOUS REPRODUCTION NUMBER", align = "high"),
      plotLines = list(
        list(label = list(text = "AMBANG SASARAN"),
             dashStyle = "Dash",
             color = "#B22222",
             width = 2,
             value = 1)),
      min = 0
    ) %>%
    hc_title(text = glue("POLA PERKEMBANGAN {toupper(which)} COVID-19 DI {toupper(location)}")) %>%
    hc_subtitle(text = glue("MODEL DIPERBAHARUI: {last_updated}")) %>%
    hc_tooltip(
      crosshairs = TRUE,
      shared = TRUE,
      sort = TRUE,
      table = FALSE
    ) %>%
    hc_add_theme(hc_theme_smpl())


  return(res)
}
