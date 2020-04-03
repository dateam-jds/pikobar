#' Project incidence
#'
#' @param incidence COVID-19 incidence object.
#' @param n_days The number of days to run simulations for.
#' @param si_mean The mean of serial interval.
#' @param si_sd The standard devation of serial interval.
#'
#' @importFrom dplyr transmute
#' @importFrom tibble as_tibble
#' @importFrom incidence cumulate
#' @importFrom earlyR get_R sample_R
#' @importFrom projections project
#' @importFrom glue glue
#'
#' @return A list of R estimate, daily prohected incidence, and cumulative projected incidence.
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
#' covid_projection
project_incidence <- function(incidence, n_days = 21, si_mean = 4.8, si_sd = 2.3) {
  if (!inherits(incidence, "covid19_incidence")) {
    stop("Please supply a valid COVID-19 incidence.", call. = FALSE)
  }

  which <- attr(incidence, "which")
  location <- attr(incidence, "location")

  last_cumulative_incidence <- sum(incidence$counts)

  r_estimate <-
    get_R(incidence,
      si_mean = si_mean,
      si_sd = si_sd
    )

  r_sampled_estimate <-
    sample_R(r_estimate, n = 1000)

  projected_incidence <-
    project(
      incidence,
      R = r_sampled_estimate,
      n_sim = 1000,
      si = r_estimate$si,
      n_days = n_days
    )

  daily_projected_incidence <-
    projected_incidence %>%
    apply(1, quantile, c(0.025, 0.5, 0.975)) %>%
    t() %>%
    as_tibble(rownames = "date") %>%
    transmute(
      date = as.Date(date),
      lower_bound = ceiling(`2.5%`),
      incidence = ceiling(`50%`),
      upper_bound = ceiling(`97.5%`)
    )

  cumulative_projected_incidence <-
    projected_incidence %>%
    cumulate() %>%
    apply(1, quantile, c(0.025, 0.5, 0.975)) %>%
    t() %>%
    as_tibble(rownames = "date") %>%
    transmute(
      date = as.Date(date),
      lower_bound = ceiling(`2.5%`) + last_cumulative_incidence,
      incidence = ceiling(`50%`) + last_cumulative_incidence,
      upper_bound = ceiling(`97.5%`) + last_cumulative_incidence
    )

  res <- list(
    si_mean = si_mean,
    si_sd = si_sd,
    r_estimate = r_estimate$R_ml,
    daily = daily_projected_incidence,
    cumulative = cumulative_projected_incidence,
    last_updated = format(Sys.time(), tz = "Asia/Jakarta", usetz = TRUE)
  )

  attr(res, "which") <- which
  attr(res, "location") <- location
  class(res) <- append(class(res), "covid19_projection")

  return(res)
}
