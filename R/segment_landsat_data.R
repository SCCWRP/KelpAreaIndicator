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
#' @param ... unused
#' @param segment_id either "all" or a character vector containing the subset of
#' `segment_id`'s of interest, e.g. `c("CA_71", "CA_78")`
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
segment_landsat_data <- function(lter_file_path, kelp_segments_file_path, ..., segment_id = "all") {
  nc_file <- ncdf4::nc_open(lter_file_path)

  #lon, lat, area, year from netCDF file, as matrices
  lat <- ncdf4::ncvar_get(nc_file, varid = "latitude")
  lon <- ncdf4::ncvar_get(nc_file, varid = "longitude")
  area <- ncdf4::ncvar_get(nc_file, varid = "area")
  nc_year <- ncdf4::ncvar_get(nc_file, varid = "year")

  # read in the kelp segments shapefile
  kelp_segments <- sf::st_read(kelp_segments_file_path)

  # if a segment_id or subset of segment_id's are specified, make sure
  # that they are all in the kelp_segments shapefile
  if (segment_id != "all") {
    if (!all(segment_id %in% kelp_segments$Segment_ID)) {
      stop("not all values in `segment_id` are present in the kelp segments shapefile")
    }
    kelp_segments <- kelp_segments |>
      dplyr::filter(Segment_ID %in% segment_id)
  }

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
