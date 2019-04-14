geom_string <- function(mapping = NULL, data = NULL, ...,
                      segments = NULL,
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomString,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      segments = segments,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomString <- ggplot2::ggproto("GeomString", ggplot2::Geom,
                    non_missing_aes = c("size", "line.colour"),
                    default_aes = ggplot2::aes(
                      line.colour = "black", size = 0.5,
                      alpha = NA, stroke = 0.5
                    ),
                    setup_data = function(data, params) {
                      segments <- calc_offsets(params$segments, "segment_id", "min", "max")
                      segments$xend <- max(segments$offset) + segments$max[which.max(segments$offset)]
                      data <- data[match(unique(data$y), data$y),]
                      data <- merge(data, segments)
                      data$x <- 0
                      # browser()
                      data$yend <- data$y
                      data$group <- 1
                      # browser()
                      data
                    },
                    draw_group = function(data, panel_scales, coord, segments) {
                      browser()
                      data$colour <- data$line.colour
                      grid::gList(
                        ggplot2::GeomSegment$draw_panel(data, panel_scales, coord)
                      )
                    },
                    required_aes = c("x", "y", "segment_id"),

                    draw_key = ggplot2::draw_key_path
)

# non_missing_aes = c("size", "shape"),
# default_aes = aes(
#   segments = NULL, segment_id = NULL,
#   shape = 19, colour = "black", size = 0.5, fill = NA,
#   alpha = NA, stroke = 0.5
# ),
#
#


# draw_group = function(data, panel_scales, coord,
#                       segments = segments, segment_id = segment_id) {
#
#   points <- data
#   points$colour <- point.colour %||% data$colour
#   points$size <- point.size %||% (data$size * 2.5)
#
#   gList(
#     ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
#     ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
#   )
#
# },
#
# draw_key = draw_key_point
# )
