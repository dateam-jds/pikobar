#' Show static projection plot
#'
#' @param projection COVID-19 projection.
#' @param value Whether to plot daily incidence or cumulative incidence. Valid values are "daily" and "cumulative".
#'
#' @importFrom rlang arg_match
#' @importFrom glue glue
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point labs scale_x_date guide_axis theme ggsave
#' @importFrom ggrepel geom_label_repel
#' @importFrom scales label_date
#' @importFrom hrbrthemes theme_ipsum_rc
#'
#' @return a ggplot2 object.
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
#' show_projection_s(covid_projection)
show_projection_s <- function(projection, value = "cumulative") {
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
  txt_subtitle <- glue("SERIAL INTERVAL (SI): {projection$si_mean} HARI (STD. DEVIASI={projection$si_sd} HARI)\nEFFECTIVE REPRODUCTION NUMBER (R): {round(projection$r_estimate, 2)}\nMODEL DIPERBAHARUI: {last_updated}")
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
    ggplot(aes(date, incidence, group = 1)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
      fill = "violet",
      alpha = 0.4
    ) +
    geom_line(size = 0.8, colour = "grey30") +
    geom_point(size = 3, colour = "mediumvioletred") +
    geom_label_repel(aes(label = round(incidence)),
      direction = "y",
      family = "Roboto Condensed"
    ) +
    scale_x_date(
      breaks = "2 days",
      guide = guide_axis(check.overlap = TRUE),
      labels = label_date(format = "%e %b")
    ) +
    labs(
      x = NULL,
      y = "JUMLAH KASUS",
      title = txt_title,
      subtitle = txt_subtitle,
      caption = txt_caption
    ) +
    theme_ipsum_rc(15, grid = "XY") +
    theme(plot.title.position = "plot")

  return(res)
}
