#' Plot ecdf for kelp segments
#'
#' @inheritParams get_kelp_status
#' @inheritParams rlang::args_dots_empty
#' @param years Which years to plot. Default is all years present in `annual_time_series`
#' @param ylim Restrictions on the y axis (% of historical median). For visual clarity
#' only. Does not affect any calculations
#'
#' @return ggplot object of the cumulative distributions for each year of kelp segments
#' @export
plot_cumulative_dist <- function(
    annual_time_series,
    kelp_presence,
    ...,
    years = unique(annual_time_series$year),
    ylim = c(0, 200)) {

  rlang::check_dots_empty()
  plot_data <- annual_time_series |>
    dplyr::inner_join(kelp_presence, by = "Segment_ID") |>
    dplyr::filter(presence == "Kelp", !is.na(area_hist), year %in% years)

  median_data <- plot_data |>
    dplyr::group_by(year) |>
    dplyr::summarize(area_hist = quantile(area_hist, probs = 0.5, type = 3))

  ggplot2::ggplot(plot_data, ggplot2::aes(y = area_hist, group = year)) +
    ggplot2::stat_ecdf(pad = FALSE, direction = "mid") +
    ggplot2::geom_point(
      data = median_data,
      mapping = ggplot2::aes(x = 0.5, y = area_hist),
      color = "red"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, by = 0.2),
      expand = ggplot2::expansion(mult = 0.005)
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.005)) +
    ggplot2::coord_cartesian(ylim = ylim, xlim = c(0, 1)) +
    ggplot2::labs(x = "Fraction of Kelp Coastal Segments", y = "Percent of Historical") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
