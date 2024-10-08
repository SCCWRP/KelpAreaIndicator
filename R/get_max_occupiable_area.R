#' Calculate the maximum occupiable kelp area
#'
#' @description
#' Maximum occupiable kelp area is the maximum possible area that kelp has ever
#' occupied in the landsat area data, e.g. if a pixel showed 0.003 km2 kelp coverage once in the
#' 40 year time series, and 0 km2 the remainder of the time, the maximum occupiable
#' kelp area for that pixel is 0.003 km2. For an entire segment, the maximum occupiable kelp area
#' is the sum of each of its pixel's maximum occupiable kelp area.
#'
#' @inheritParams extract_time_series
#'
#' @export
#' @return A dataframe with the maximum occupiable area for each kelp segment
get_max_occupiable_area <- function(segmented_landsat_data) {
  # set up dataframe to process
  max_occupiable_kelp_area <- data.frame(Segment_ID = segmented_landsat_data$Segment_ID)

  # for the entire time series for each pixel, get the maximum area
  max_occupiable_tmp <- segmented_landsat_data |>
    dplyr::select(-c(Segment_ID, lon, lat))

  max_occupiable_kelp_area$max_occupiable <- do.call(
    function(...) pmax(..., na.rm = TRUE),
    max_occupiable_tmp
  )

  # sum together all the maximum areas in each segment
  max_occupiable_kelp_area <- max_occupiable_kelp_area |>
    dplyr::group_by(Segment_ID) |>
    dplyr::summarize(max_occupiable = sum(max_occupiable, na.rm = TRUE) / 1e6)

  max_occupiable_kelp_area
}
