#' Inspect incidence
#'
#' @param data COVID-19 data.
#' @param which Which category do you want to plot? Valid values are "positif", "sembuh", "meninggal". For Jawa Barat Data, two additional values are valid: "odp" and "pdp".
#'
#' @importFrom rlang arg_match .data
#' @importFrom dplyr select pull
#' @importFrom tidyr uncount
#' @importFrom incidence incidence
#'
#' @return A COVID-19 incidence object.
#' @export
#'
#' @examples
#' # Fetch data
#' daily <- fetch_data("Jawa Barat")
#'
#' # Inspect incidence
#' covid_incidence <-
#'   inspect_incidence(daily, "positif")
#' covid_incidence
inspect_incidence <- function(data, which = "positif") {
  which <- arg_match(which, colnames(data)[-1])

  res <-
    data %>%
    select(.data$tanggal, incidence = which) %>%
    uncount(weights = .data$incidence) %>%
    pull(.data$tanggal) %>%
    incidence()

  attr(res, "which") <- which
  attr(res, "location") <- attr(data, "location")
  class(res) <- append(class(res), "covid19_incidence")

  return(res)
}
