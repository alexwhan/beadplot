geom_segment <- function(mapping = NULL, data = NULL, ...,
                      segments = NULL,
                      na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomSegment,
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

#' @rdname beadplot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSegment <- ggproto("GeomSegment", Geom,
                    non_missing_aes = c("segment.fill", "size"),
                    default_aes = aes(segment.fill = "grey", size = 0.5,
                                      alpha = NA),
                    setup_data = function(data, params) {
                      # browser()
                      data <- data[match(unique(data$segment_id), data$segment_id),]
                      data <- data[order(data$segment_id),]
                      segments <- params$segments[order(params$segments$segment_id),]
                      segments <- calc_offsets(segments, "segment_id", "min", "max")
                      rects <- segments
                      rects$xmin <- rects$offset
                      rects$xmax <- rects$offset + (rects$max - rects$min)
                      rects$PANEL <- data$PANEL[1]
                      rects$group <- 1
                      rects$size <- data$size
                      browser()
                      data$fill <- data$segment.fill
                      rects$fill <- data$fill
                      rects$segment.fill <- data$segment.fill
                      # browser()
                      data <- rects
                    },
                    draw_group = function(data, panel_scales, coord, segments) {
                      browser()
                      data$ymin <- panel_scales$y.range[1]
                      data$ymax <- panel_scales$y.range[2]
                      # browser()
                      grid::gList(
                        ggplot2::GeomRect$draw_panel(data, panel_scales, coord)
                      )
                    },
                    required_aes = c("segment_id"),

                    draw_key = draw_key_rect
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
)
