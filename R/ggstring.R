StatString <- ggplot2::ggproto("StatString", ggplot2::Stat,
                      required_aes = c("x", "y"),

                      setup_params = function(data, params) {
                        segments <- calc_offsets(params$segments, "segment_id", "min", "max")
                        list(
                          min = min(segments$offset),
                          max = max(segments$offset) + max(segments$diff),
                          na.rm = params$na.rm
                        )
                      },

                      compute_group = function(data, scales, min, max, segments) {
                        data.frame(x = c(min, max), y = data$y)
                      }
)

#' A stat to calculate string extents across segments
#'
#' @inheritParams ggplot2::stat_identity
#' @param segments A data.frame describing max and min of each segment
#'
#' @export
#' @examples
#' ggplot(data, aes(dist, y)) +
#'   stat_string(segments = segments)

stat_string <- function(mapping = NULL, data = NULL, geom = "line",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, segments = NULL,
                        ...) {
  ggplot2::layer(
    stat = StatString, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(segments = segments, na.rm = na.rm, ...)
  )
}
