#' Assign segment ID's to each pixel in the landsat data according to a
#' given shapefile
#'
#' @description
#' This function assigns to each Landsat pixel segment ID's derived from a shapefile of polygons with a
#' Segment_ID attribute. This shapefile describes segments of interest along the west coast of the US.
#' This function assumes that the Landsat data starts with quarter 1 of the first year of interest.
#'
#' @param lter_file_path character string path to the landsat netCDF file
#' @param kelp_segments_file_path character string path to the kelp segments shapefile
#' @inheritParams rlang::args_dots_empty
#' @param fractional_pixels whether to consider fractional area values of pixels, e.g.
#' if a pixel in the Landsat data has a value of 66 m^2, setting `fractional_pixels = FALSE`
#' will treat the pixel as 900 m^2 instead
#'
#' @export
#' @return A dataframe with rows for each pixel, with the following columns:
#'  * `Segment_ID`: The ID of the kelp area segment
#'  * `lon`: The longitude of the Landsat pixel
#'  * `lat`: The latitude of the Landsat pixel
#'  * `Q1.1984`: The kelp area in m^2 of the given pixel for quarter 1 of 1984
#'  * ... : subsequent quarters and years
#'
#' Segments that do not have any pixels ever are represented as rows of `NA` values
segment_landsat_data <- function(
    lter_file_path,
    kelp_segments_file_path,
    ...,
    fractional_pixels = TRUE) {

  rlang::check_dots_empty()

  nc_file <- ncdf4::nc_open(lter_file_path)

  #lon, lat, area, year from netCDF file, as matrices
  lat <- ncdf4::ncvar_get(nc_file, varid = "latitude")
  lon <- ncdf4::ncvar_get(nc_file, varid = "longitude")
  area <- ncdf4::ncvar_get(nc_file, varid = "area")
  nc_year <- ncdf4::ncvar_get(nc_file, varid = "year")

  if (!fractional_pixels) {
    area[area > 0] <- 900
  }

  # read in the kelp segments shapefile
  kelp_segments <- sf::st_read(kelp_segments_file_path)

  # set up data frame to be processed
  # coerces the "area" matrix to dataframe columns with area.1 format
  # where .1 is the matrix column number
  segmented_landsat_data <- data.frame(
    lon = lon,
    lat = lat,
    area = area
  )

  # rename the area column names to list quarter and year.
  # assumes data starts at quarter 1 of the first year and is
  # in chronological order
  names(segmented_landsat_data)[3:length(names(segmented_landsat_data))] <- paste(
    c("Q1", "Q2", "Q3", "Q4"),
    nc_year,
    sep = "."
  )

  # spatial join the landsat data with the segment shapefile so that
  # each recorded pixel is assigned a kelp segment.
  # not all segments will have pixels, and so will be left out of the
  # initial spatial join. so, use a right join on the kelp_segments
  # shapefile to ensure every Segment_ID of interest is represented in
  # the final data output with NA rows
  segmented_landsat_data <- segmented_landsat_data |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326)) |>
    sf::st_join(kelp_segments, join = sf::st_within, left = FALSE)

  segmented_landsat_data <- segmented_landsat_data |>
    dplyr::mutate(
      lon = sf::st_coordinates(segmented_landsat_data)[, 1],
      lat = sf::st_coordinates(segmented_landsat_data)[, 2]
    ) |>
    sf::st_drop_geometry() |>
    dplyr::right_join(sf::st_drop_geometry(kelp_segments), by = "Segment_ID") |>
    dplyr::relocate(Segment_ID, lon, lat) |>
    dplyr::as_tibble()

  segmented_landsat_data
}
