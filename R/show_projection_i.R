#' Show interactive projection plot
#'
#' @param projection COVID-19 projection.
#' @param value Whether to plot daily incidence or cumulative incidence. Valid values are "daily" and "cumulative".
#'
#' @importFrom rlang arg_match
#' @importFrom glue glue
#' @importFrom highcharter hchart hcaes hc_add_series hc_xAxis hc_yAxis hc_title hc_subtitle hc_credits hc_tooltip hc_add_theme hc_theme_smpl
#'
#' @return a highchart object.
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
#' show_trend_s(covid_trend)
#'
#' # Project future incidence
#' covid_projection <-
#'   project_incidence(
#'     covid_incidence,
#'     n_days = 21
#'   )
#' show_projection_i(covid_projection)
show_projection_i <- function(projection, value = "cumulative") {
  if (!inherits(projection, "covid19_projection")) {
    stop("Please supply a valid COVID-19 projection.", call. = FALSE)
  }

  value <- arg_match(value, c("cumulative", "daily"))
  which <- attr(projection, "which")
  location <- attr(projection, "location")

  projection_data <- projection$daily
  last_updated <- projection$last_updated %>%
    as.POSIXct() %>%
    format(format = "%e %b %Y %H:%M", tz = "Asia/Jakarta", usetz = TRUE)

  txt_title <- glue("PROYEKSI ANGKA HARIAN {toupper(which)} COVID-19 DI {toupper(location)}")
  txt_subtitle <- glue("SERIAL INTERVAL (SI): {projection$si_mean} HARI (STD. DEVIASI={projection$si_sd} HARI)<br>EFFECTIVE REPRODUCTION NUMBER (R): {round(projection$r_estimate, 2)}<br>MODEL DIPERBAHARUI: {last_updated}")
  txt_caption <- glue("SUMBER DATA: DINAS KESEHATAN PROVINSI JAWA BARAT")

  if (location == "Indonesia") {
    txt_caption <- glue("SUMBER DATA: KAWALCOVID19.ID")
  }

  if (value == "cumulative") {
    projection_data <- projection$cumulative
    txt_title <- glue("PROYEKSI JUMLAH KUMULATIF {toupper(which)} COVID-19 DI {toupper(location)}")
  }

  res <-
    projection_data %>%
    hchart(
      "arearange",
      hcaes(date, low = lower_bound, high = upper_bound),
      name = "95% CI",
      color = "#C71585",
      fillOpacity = 0.3
    ) %>%
    hc_add_series(projection_data,
      "line",
      hcaes(date, incidence),
      name = "MEDIAN",
      color = "#B22222"
    ) %>%
    hc_xAxis(title = list(text = NULL)) %>%
    hc_yAxis(
      title = list(text = "JUMLAH KASUS", align = "high"),
      min = 0
    ) %>%
    hc_title(text = txt_title) %>%
    hc_subtitle(text = txt_subtitle) %>%
    hc_credits(enabled = TRUE, text = txt_caption) %>%
    hc_tooltip(
      crosshairs = TRUE,
      shared = TRUE,
      sort = TRUE,
      table = FALSE
    ) %>%
    hc_add_theme(hc_theme_smpl())

  return(res)
}
