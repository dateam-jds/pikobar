#' Show static beds projection plot
#'
#' @param projection Beds projection.
#'
#' @importFrom rlang arg_match
#' @importFrom glue glue
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point labs scale_x_date guide_axis expand_limits theme ggsave
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
#'
#' # Inspect daily trend
#' covid_trend <-
#'   inspect_trend(covid_incidence)
#'
#' # Project future incidence
#' covid_projection <-
#'   project_incidence(
#'     covid_incidence,
#'     n_days = 21
#'   )
#'
#' # Project beds need
#' beds_projection <-
#'   project_beds(
#'     projection = covid_projection,
#'     los = 13,
#'     n_sim = 10
#'   )
#' show_beds_projection_s(beds_projection)
show_beds_projection_s <- function(beds_projection) {
  if (!inherits(beds_projection, "beds_projection")) {
    stop("Please supply a valid beds projection.", call. = FALSE)
  }

  which <- attr(beds_projection, "which")
  location <- attr(beds_projection, "location")

  projection_data <- beds_projection[["projection"]]
  los <- beds_projection[["los"]]
  last_updated <- beds_projection[["last_updated"]] %>%
    as.POSIXct() %>%
    format(format = "%e %b %Y %H:%M", tz = "Asia/Jakarta", usetz = TRUE)

  txt_title <- glue("PROYEKSI KEBUTUHAN HARIAN KASUR RUMAH SAKIT DI {toupper(location)}")
  txt_subtitle <- glue("ASUMSI LAMA WAKTU INAP: {los} HARI\nMODEL DIPERBAHARUI: {last_updated}")
  txt_caption <- glue("SUMBER DATA: DINAS KESEHATAN PROVINSI JAWA BARAT")

  if (location == "Indonesia") {
    txt_caption <- glue("SUMBER DATA: KAWALCOVID19.ID")
  }

  res <-
    projection_data %>%
    ggplot(aes(date, n_beds, group = 1)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                fill = "yellow2",
                alpha = 0.4
    ) +
    geom_line(size = 0.8, colour = "grey30") +
    geom_point(size = 3, colour = "darkorange1") +
    geom_label_repel(aes(label = round(n_beds)),
                     direction = "y",
                     family = "Roboto Condensed"
    ) +
    scale_x_date(
      breaks = "2 days",
      guide = guide_axis(check.overlap = TRUE),
      labels = label_date(format = "%e %b")
    ) +
    expand_limits(y = 0) +
    labs(
      x = NULL,
      y = "JUMLAH KASUR",
      title = txt_title,
      subtitle = txt_subtitle,
      caption = txt_caption
    ) +
    theme_ipsum_rc(15, grid = "XY") +
    theme(plot.title.position = "plot")

  return(res)
}
