geom_bead <- function(mapping = NULL, data = NULL, ...,
                      segments = NULL,
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

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
      segments = segments,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBead <- ggproto("GeomBead", Geom,
                    non_missing_aes = c("shape", "size", "segment.fill"),
                    default_aes = aes(
                      shape = 19, colour = "black", fill = NA, size = 0.5,
                      alpha = NA, stroke = 0.5, segment.fill = "grey"
                    ),
                    setup_data = function(data, params) {
                      segments <- calc_offsets(params$segments, "segment_id", "min", "max")
                      data <- merge(data, segments)
                      data$x <- data$x + data$offset
                      data$xend <- max(data$offset) + data$max[which.max(data$offset)]
                      data$yend <- data$y
                      data$group <- data$y
                      data
                    },
                    draw_group = function(data, panel_scales, coord, segments) {
                      browser()
                      segments <- calc_offsets(segments, "segment_id", "min", "max")
                      segments <- merge(segments, data)
                      segments$x <- 0
                      data$size <- data$size * 5
                      browser()
                        grid::gList(
                          ggplot2::GeomPoint$draw_panel(data, panel_scales, coord)
                        )
                    },
                    required_aes = c("x", "y", "segment_id"),

                    draw_key = draw_key_point
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
