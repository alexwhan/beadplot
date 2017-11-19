StatString <- ggproto("StatString", Stat,
                      required_aes = c("x", "y"),

                      setup_params = function(data, params) {
                        browser()
                        offsets <- calc_offsets(params$offsets, "group", "min", "max")
                        list(
                          min = min(offsets$offset),
                          max = max(offsets$offset) + max(offsets$diff),
                          na.rm = params$na.rm
                        )
                      },

                      compute_group = function(data, scales, min, max, offsets) {
                        data.frame(x = c(min, max), y = data$y)
                      }
)

#' @inheritParams ggplot2::stat_identity
#' @param offsets A data.frame describing max and min of each group
#' @examples
#' ggplot(data, aes(dist, y)) +
#'   stat_string(offsets = segments)

stat_string <- function(mapping = NULL, data = NULL, geom = "line",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, offsets = NULL,
                        ...) {
  ggplot2::layer(
    stat = StatString, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(offsets = offsets, na.rm = na.rm, ...)
  )
}
