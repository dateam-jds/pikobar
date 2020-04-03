#' Inspect trend
#'
#' @param incidence A COVID-19 incidence object.
#' @param si_mean The mean of serial interval.
#' @param si_sd The standard devation of serial interval.
#'
#' @importFrom dplyr transmute left_join
#' @importFrom tibble tibble
#' @importFrom EpiEstim estimate_R make_config
#' @importFrom ggplot2 layer_data
#'
#' @return A tibble of instantenous reproduction number estimates.
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
#' covid_trend
inspect_trend <- function(incidence,
                          si_mean = 4.8,
                          si_sd = 2.3) {
  if (!inherits(incidence, "covid19_incidence")) {
    stop("Please supply a valid COVID-19 incidence.", call. = FALSE)
  }

  incid_dat <-
    tibble(
      dates = incidence$dates,
      I = as.integer(incidence$counts)
    )

  t_start <- seq(7, nrow(incid_dat) - 7)
  t_end <- t_start + 7

  estimation <-
    estimate_R(
      incid = incid_dat,
      method = "parametric_si",
      config = make_config(
        list(
          mean_si = si_mean,
          std_si = si_sd,
          t_start = t_start,
          t_end = t_end
        )
      )
    )

  r_estimates <-
    tibble(date = estimation$dates) %>%
    left_join(
      layer_data(EpiEstim:::plot.estimate_R(estimation, "R"), 2) %>%
        transmute(
          date = as.Date(x, origin = "1970-01-01"),
          r = round(y, 2)
        ),
      by = "date"
    ) %>%
    left_join(
      layer_data(EpiEstim:::plot.estimate_R(estimation, "R"), 1) %>%
        transmute(
          date = as.Date(x, origin = "1970-01-01"),
          lower_bound = round(ymin, 2),
          upper_bound = round(ymax, 2)
        ),
      by = "date"
    )

  res <-
    list(
      r_estimates = r_estimates,
      last_updated = format(Sys.time(), tz = "Asia/Jakarta", usetz = TRUE)
    )

  attr(res, "which") <- attr(incidence, "which")
  attr(res, "location") <- attr(incidence, "location")
  class(res) <- append(class(res), "covid19_trend")

  return(res)
}
