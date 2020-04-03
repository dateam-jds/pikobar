#' Fetch COVID-19 data
#'
#' @param location Which location data do you want to fetch? Currently the valid values are "Jawa Barat" and "Indonesia"
#'
#' @importFrom rlang arg_match
#' @importFrom httr GET content
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider drop_na
#' @importFrom dplyr transmute filter case_when
#'
#' @return a tibble with columns indicating daily incidence of "positif", "sembuh", "meninggal". If the location is Jawa Barat, two additional columns are added: "odp" and "pdp".
#' @export
#'
#' @examples
#' (jabar <- fetch_data(location = "Jawa Barat"))
#' (indonesia <- fetch_data(location = "Indonesia"))
fetch_data <- function(location = "Jawa Barat") {
  if (length(location) != 1) {
    stop("Please supply one value only.", call. = FALSE)
  }

  location <-
    arg_match(location, values = c("Jawa Barat", "Indonesia"))

  if (location == "Jawa Barat") {
    jabar_raw <-
      GET(
        "https://covid19-public.digitalservice.id/api/v1/rekapitulasi/jabar/harian?level=prov"
      ) %>%
      content()

    res <-
      jabar_raw[[c("data", "content")]] %>%
      enframe(name = NULL) %>%
      unnest_wider(value) %>%
      filter(tanggal >= as.Date("2020-03-06")) %>%
      transmute(
        tanggal = as.Date(tanggal),
        pdp = case_when(
          tanggal <= as.Date("2020-03-09") ~ 0,
          TRUE ~ as.double(pdp)
        ),
        odp = case_when(
          tanggal <= as.Date("2020-03-09") ~ 0,
          TRUE ~ as.double(odp)
        ),
        positif,
        sembuh,
        meninggal
      ) %>%
      drop_na()
    # filter(tanggal < as.Date(format(Sys.time(), tz = "Asia/Jakarta")))
  } else if (location == "Indonesia") {
    ina_raw <-
      GET("https://indonesia-covid-19.mathdro.id/api/harian") %>%
      content()

    res <-
      ina_raw[["data"]] %>%
      enframe(name = NULL) %>%
      unnest_wider(value) %>%
      transmute(
        tanggal = as.Date(as.POSIXct(tanggal / 1000, origin = "1970-01-01")),
        positif = jumlahKasusBaruperHari,
        sembuh = jumlahKasusSembuhperHari,
        meninggal = jumlahKasusMeninggalperHari
      ) %>%
      drop_na()
    # filter(tanggal < as.Date(format(Sys.time(), tz = "Asia/Jakarta")))
  }

  attr(res, "location") <- location
  return(res)
}
