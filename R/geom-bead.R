geom_bead <- function(mapping = NULL, data = NULL,
                      na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE,  ...) {
  # browser()
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomBead,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBead <- ggplot2::ggproto("GeomBead", Geom,
                             non_missing_aes = c("shape", "size", "fill"),
                             default_aes = ggplot2::aes(
                               shape = 19,
                               colour = "black",
                               fill = "grey",
                               size = 0.5,
                               alpha = NA,
                               stroke = 0.5,
                               segment.fill = "grey"
                             ),
                             setup_data = function(data, params) {
                               data
                               data <- calc_offsets_full(data, "segment_id", "segment_min", "segment_max")
                               data$x <- data$x + data$offset
                               data$xend <- max(data$offset) + data$segment_max[which.max(data$offset)]
                               data$yend <- data$y
                               data$group <- data$y
                               data$xmin <- data$offset
                               data$xmax <- data$offset + (data$segment_max - data$segment_min)
                               rects <- data[match(unique(data$segment_id), data$segment_id),]
                               rects$group <- 1
                               rects$x <- 0
                               rects$y <- 4#data$y[!is.na(data$x)][1]
                               rects$yend <- data$y[!is.na(data$x)][1]
                               segs <- data[!is.na(data$x),]
                               segs$x <- 0
                               segs$group <- 2
                               data$group <- data$group + 2
                               data <- data[!is.na(data$x),]
                               browser()
                               dplyr::bind_rows(rects, segs, data)
                             },

                             draw_group = function(data, panel_scales, coord) {
                               browser()
                               if(data$group[1] == 1) {

                                 data$ymin <- panel_scales$y.range[1]
                                 data$ymax <- panel_scales$y.range[2]
                                 grid::gList(
                                   ggplot2::GeomRect$draw_panel(data, panel_scales, coord)
                                 )
                               } else if(data$group[1] == 2) {
                                 grid::gList(
                                   ggplot2::GeomSegment$draw_panel(data, panel_scales, coord)
                                 )
                               } else {
                                 data$size <- data$size * 5
                                 grid::gList(
                                   ggplot2::GeomPoint$draw_panel(data, panel_scales, coord)
                                 )
                               }
                               # grid::gList(ggplot2::GeomPoint$draw_panel(data, panel_scales, coord))
                             },
                             required_aes = c("x", "y", "segment_id", "segment_min", "segment_max"),

                             draw_key = ggplot2::draw_key_point
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
#   grid::gList(
#     ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
#     ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
#   )
#
# },
#
# draw_key = draw_key_point
# )

GeomBead <- ggproto("GeomBead", Geom,
                    required_aes = c("x", "y"),
                    non_missing_aes = c("size", "shape"),
                    default_aes = aes(
                      shape = 19, colour = "black", size = 0.5, fill = NA,
                      alpha = NA, stroke = 0.5
                    ),



                    draw_group = function(data, panel_scales, coord,
                                          point.colour = NULL, point.size = NULL,
                                          horizontal = FALSE) {

                      points <- data
                      points$colour <- point.colour %||% data$colour
                      points$size <- point.size %||% (data$size * 2.5)

                      grid::gList(
                        ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
                        ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
                      )

                    },

                    draw_key = draw_key_point
)
