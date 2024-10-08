#' Get kelp presence within each segment
#'
#' @description
#' Many kelp segments may have only a few pixels ever detected within them, so this function serves
#' to classify these different kelp segments, based on the area that each detected pixel occupies
#' relative to the area of the entire kelp segment.
#'
#' @inheritParams extract_time_series
#' @inheritParams segment_landsat_data
#' @inheritParams rlang::args_dots_empty
#' @param no_kelp_bound Upper bound, in percent of segment area, for the "No Historical Kelp"
#' category. Default is 0.02
#' @param ephemeral_kelp_bound Upper bound, in percentage of segment area, for the "Ephemeral
#' Kelp" category. Default is 0.15
#'
#' @return A dataframe with the following columns:
#'
#'  * `Segment_ID`: The ID of the kelp area segment
#'  * `segment_area`: The area in km2 of the kelp segment
#'  * `pixel_percent`: The percentage of the kelp segment's area that the total number of pixels
#'  ever detected occupy. This calculation assumes each pixel contains the full 900 m2 of kelp
#'  area
#'  * `presence`: The category of kelp presence for each segment.
#'      * "No Historical Kelp": `pixel_percent` <= `no_kelp_bound` OR `pixel_percent` is `NA`
#'      * "Ephemeral Kelp": `pixel_percent` <= `ephemeral_kelp_bound`
#'      * "Kelp": otherwise
#' @export
get_kelp_presence <- function(
    segmented_landsat_data,
    kelp_segments_file_path,
    ...,
    no_kelp_bound = 0.02,
    ephemeral_kelp_bound = 0.15) {

  rlang::check_dots_empty()
  kelp_presence <- data.frame(Segment_ID = segmented_landsat_data$Segment_ID)

  # indicate whether a pixel has ever had any kelp area detected across the
  # time series
  kelp_presence$is_present <- segmented_landsat_data |>
    dplyr::select(-c(Segment_ID, lon, lat)) |>
    apply(1, function(x) {
      as.numeric(any(x > 0))
    })

  # count up the number of distinct pixels ever detected in each segment
  kelp_presence <- kelp_presence |>
    dplyr::group_by(Segment_ID) |>
    dplyr::summarize(num_pixels = sum(is_present))

  kelp_segments <- sf::st_read(kelp_segments_file_path, quiet = TRUE)

  # calculate the area of each polygon, convert to km2
  kelp_presence <- kelp_segments |>
    dplyr::mutate(
      segment_area = as.numeric(sf::st_area(kelp_segments)) / 1e6 # in km2
    ) |>
    sf::st_drop_geometry() |>
    # add in the number of pixels, left join to include kelp segments that
    # never have any pixels
    dplyr::left_join(kelp_presence, by = "Segment_ID") |>
    dplyr::mutate(
      # assume each pixel has a max of 900 m2 area for this calculation
      pixel_area = num_pixels * 900 / 1e6, # in km2
      pixel_percent = pixel_area / segment_area * 100,
      presence = dplyr::case_when(
        pixel_percent <= no_kelp_bound | is.na(pixel_percent) ~ "No Historical Kelp",
        pixel_percent <= ephemeral_kelp_bound ~ "Ephemeral Kelp",
        .default = "Kelp"
      )
    ) |>
    dplyr::select(-c(num_pixels, pixel_area)) |>
    dplyr::as_tibble()

  kelp_presence
}
