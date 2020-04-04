#' Show incidence plot
#'
#' @param incidence A COVID-19 incidence object.
#' @importFrom ggplot2 ggplot aes geom_col scale_x_date labs theme
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
show_incidence_s <- function(incidence) {
  if (!inherits(incidence, "covid19_incidence")) {
    stop("Please supply a valid COVID-19 incidence.", call. = FALSE)
  }

  which <- attr(incidence, "which")
  location <- attr(incidence, "location")

  res <-
    tibble(
      date = incidence$dates,
      incidence = as.integer(incidence$counts)
    ) %>%
    ggplot(aes(date, incidence)) +
    geom_col(fill = "steelblue") +
    scale_x_date(breaks = "3 days",
                 labels = label_date(format = "%e %b"),
                 expand = c(0.005, 0.005)) +
    labs(
      x = NULL,
      y = "JUMLAH KASUS TERKONFIRMASI",
      title = glue(
        "ANGKA HARIAN KASUS {toupper(which)} COVID-19 DI {toupper(location)}"
      ),
      caption = switch(location,
        "Indonesia" = "SUMBER DATA: KAWALCOVID19.ID",
        "Jawa Barat" = "SUMBER DATA: DINAS KESEHATAN PROVINSI JAWA BARAT"
      )
    ) +
    theme_ipsum_rc(base_size = 10, grid = "Y") +
    theme(plot.title.position = "plot")
  return(res)
}
