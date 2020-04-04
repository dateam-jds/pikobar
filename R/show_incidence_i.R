#' Show interactive incidence plot
#'
#' @param incidence A COVID-19 incidence object.
#' @importFrom highcharter hchart hcaes hc_xAxis hc_yAxis hc_title hc_credits hc_add_theme hc_theme_smpl
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
#' show_incidence_i(covid_incidence)
show_incidence_i <- function(incidence) {
  if (!inherits(incidence, "covid19_incidence")) {
    stop("Please supply a valid COVID-19 incidence.", call. = FALSE)
  }

  which <- attr(incidence, "which")
  location <- attr(incidence, "location")

  res <-
    tibble(date = incidence$dates,
           incidence = as.integer(incidence$counts)) %>%
    hchart("column",
           hcaes(date, incidence),
           name = toupper(which),
           color = "#4682B4") %>%
    hc_xAxis(title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = "JUMLAH KASUS TERKONFIRMASI",
                          align = "high"),
             min = 0) %>%
    hc_title(text = glue("ANGKA HARIAN KASUS {toupper(which)} COVID-19 DI {toupper(location)}")) %>%
    hc_credits(
      enabled = TRUE,
      text = switch(
        location,
        "Indonesia" = "SUMBER DATA: KAWALCOVID19.ID",
        "Jawa Barat" = "SUMBER DATA: DINAS KESEHATAN PROVINSI JAWA BARAT")) %>%
    hc_add_theme(hc_theme_smpl())
  return(res)
}
