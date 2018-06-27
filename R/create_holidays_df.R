#' Create data frame containing holiday dates
#'
#' This is is used with the prophet package to do forecasting of time series.
#' Adding holiday information may improve the accuracy of the forecast.
#'
#' @param begin Date indicating when we should start looking for holidays.
#' @param end Date indicating when we should stop looking for holidays.
#' @return A data frame with two columns: ds and holiday. The former contains a
#'   date, and the latter, a description of the holiday.
#' @export
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @examples
#' \dontrun{
#' create_df_holidays(lubridate::ymd("20180101"), lubridate::ymd("20181231"))
#' }
create_df_holidays <- function(begin, end) {
  year_begin <- year(begin)
  year_end <- year(end)
  # 1. Easter
  easter_df <- purrr::map_df(seq(year_begin,
                                 year_end),
                             function(yr) {
                                   date_easter <- tis::easter(yr) %>%
                                     as.character() %>%
                                     ymd

                                   tibble(ds = date_easter + days(c(-2, 0, 1)),
                                          holiday = c("good friday",
                                                      "easter",
                                                      "easter monday"))
                                 })

  # 2. Christmas and NY
  christ_df <- purrr::map_df(seq(year_begin,
                                 year_end),
                             function(yr) {
                               christmas <- ymd(paste0(yr, "1225"))
                               tibble(ds = c(ymd(paste0(yr, "0101")),
                                               christmas + days(c(-1, 0, 1, 6))),
                                      holiday = c("new year", "christmas eve",
                                                  "christmas", "boxing day",
                                                  "new year's eve"))
                               })

  # 3. Fixed dates
  fixed_df <- purrr::map_df(seq(year_begin,
                                year_end),
                            function(yr) {
                              tibble(ds = c(ymd(paste0(yr, "0701")),
                                              ymd(paste0(yr, "1111"))),
                                     holiday = c("canada day",
                                                 "remembrance day"))
                            })

  # 4. Monday holidays
  monday_df <- purrr::map_df(seq(year_begin,
                                 year_end),
                             function(yr) {
                               tibble(ds = c(floor_date(ymd(paste0(yr, "0221")),
                                                          unit = "week",
                                                          week_start = 1),
                                               floor_date(ymd(paste0(yr, "0524")),
                                                          unit = "week",
                                                          week_start = 1),
                                               floor_date(ymd(paste0(yr, "0807")),
                                                          unit = "week",
                                                          week_start = 1),
                                               floor_date(ymd(paste0(yr, "0907")),
                                                          unit = "week",
                                                          week_start = 1),
                                               floor_date(ymd(paste0(yr, "1014")),
                                                          unit = "week",
                                                          week_start = 1)),
                                      holiday = c("family day",
                                                  "victoria day",
                                                  "civic holiday",
                                                  "labour day",
                                                  "thanksgiving"))
                               })

  dplyr::bind_rows(
    easter_df,
    christ_df,
    fixed_df,
    monday_df
  ) %>%
    dplyr::arrange(ds)
}
