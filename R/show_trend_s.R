#' Show static trend plot
#'
#' @param trend A COVID-19 trend object.
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline geom_point scale_x_date expand_limits labs theme
#' @importFrom ggrepel geom_label_repel
#' @importFrom scales label_date
#' @importFrom hrbrthemes theme_ipsum_rc
#' @importFrom glue glue
#'
#' @return A ggplot2 object.
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
show_trend_s <- function(trend) {
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
    ggplot(aes(date, r, ymin = lower_bound, ymax = upper_bound)) +
    geom_hline(
      yintercept = 1,
      colour = "firebrick",
      linetype = "dashed",
      size = 1
    ) +
    geom_ribbon(fill = "seagreen3", alpha = 0.3, na.rm = TRUE) +
    geom_line(colour = "seagreen3", na.rm = TRUE) +
    geom_point(size = 2, colour = "seagreen3", na.rm = TRUE) +
    geom_label_repel(aes(label = r),
      direction = "y",
      family = "Roboto Condensed",
      na.rm = TRUE
    ) +
    scale_x_date(breaks = "3 days",
                 labels = label_date(format = "%e %b")) +
    expand_limits(y = 0) +
    labs(
      x = NULL,
      y = "INSTANTANEOUS REPRODUCTION NUMBER",
      title = glue("POLA PERKEMBANGAN {toupper(which)} COVID-19 DI {toupper(location)}"),
      subtitle = glue("MODEL DIPERBAHARUI: {last_updated}")
    ) +
    theme_ipsum_rc(base_size = 10, grid = "Y") +
    theme(
      plot.title.position = "plot"
    )

  return(res)
}
