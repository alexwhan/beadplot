StatBead <- ggplot2::ggproto("StatBead", ggplot2::Stat,
                      required_aes = c("x", "y", "segment_id"),

                      setup_params = function(data, params) {
                        segments <- calc_offsets(params$segments, "segment_id", "min", "max")
                        list(
                          segments = segments,
                          na.rm = params$na.rm
                        )
                      },

                      compute_group = function(data, scales, segments, segment_id) {
                        data <- merge(data, segments)
                        data$x <- data$x + data$offset
                        #browser()
                        data
                      }
)

#' A stat to calculate bead positions (with segments)
#'
#' @inheritParams ggplot2::stat_identity
#' @param segments A data.frame describing max and min of each group
#' @param segment_id A variable descriging the segment if
#'
#' @export
#' @examples
#' ggplot2::ggplot(data, ggplot2::aes(dist, y)) +
#'   stat_string(segments = segments)

stat_bead <- function(mapping = NULL, data = NULL, geom = "point",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, segments = NULL, segment_id = NULL,
                        ...) {
  ggplot2::layer(
    stat = StatBead, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(segments = segments, segment_id = segment_id, na.rm = na.rm, ...)
  )
}

