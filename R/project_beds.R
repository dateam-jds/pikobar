#' Project beds occupancy
#'
#' @param projection COVID-19 projection.
#' @param los Length of stay in hospital.
#' @param n_sim The number of times duration of hospitalisation is simulated for each admission.
#'
#' @importFrom distcrete distcrete
#' @importFrom incidence incidence get_dates
#' @importFrom projections build_projections merge_projections
#' @importFrom tibble as_tibble
#' @importFrom dplyr transmute
#'
#' @return A projection of beds occupancy.
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
#'
#' # Project beds need
#' beds_projection <-
#'   project_beds(
#'     projection = covid_projection,
#'     los = 13,
#'     n_sim = 10
#'   )
#' beds_projection
project_beds <- function(projection, los = 13, n_sim = 10) {
  # This codes are obtained and modified from https://github.com/thibautjombart/covid19_bed_occupancy

  if (!inherits(projection, "covid19_projection")) {
    stop("Please supply a valid COVID-19 projection.", call. = FALSE)
  }

  if (!is.finite(n_sim)) {
    stop("`n_sim` is not a number", call. = FALSE)
  }
  if (n_sim < 1) {
    stop("`n_sim` must be >= 1", call. = FALSE)
  }

  r_los <-
    distcrete("weibull",
              shape = 2,
              scale = los,
              w = 0,
              interval = 1)[["r"]]

  beds_projection <-
    lapply(
      c("lower_bound", "incidence", "upper_bound"),
      function(x) {
        dates <-  projection[["daily"]][["date"]]
        n_admissions <- projection[["daily"]][[x]]
        admission_dates <- rep(dates, n_admissions)
        n <- length(admission_dates)
        last_date <- max(dates)
        out <- vector(n_sim, mode = "list")
        for (j in seq_len(n_sim)) {
          los <- r_los(n)
          list_dates_beds <- lapply(seq_len(n),
                                    function(i)
                                      seq(admission_dates[i],
                                          length.out = los[i],
                                          by = 1L))
          dates_beds <- do.call(c, list_dates_beds)
          beds_days <- incidence(dates_beds)
          if (!is.null(last_date)) {
            to_keep <- get_dates(beds_days) <= last_date
            beds_days <- beds_days[to_keep, ]
          }

          out[[j]] <-
            build_projections(x = beds_days$counts,
                              dates = get_dates(beds_days))
        }

        merge_projections(out)
      }
    ) %>%
    merge_projections %>%
    apply(1, quantile, c(0.025, 0.5, 0.975)) %>%
    t() %>%
    as_tibble(rownames = "date") %>%
    transmute(
      date = as.Date(date),
      lower_bound = ceiling(`2.5%`),
      n_beds = ceiling(`50%`),
      upper_bound = ceiling(`97.5%`)
    )

  res <-
    list(
      projection = beds_projection,
      los = los,
      last_updated = projection$last_updated
    )

  attr(res, "which") <- attr(projection, "which")
  attr(res, "location") <- attr(projection, "location")
  class(res) <- append(class(res), "beds_projection")

  return(res)
}
