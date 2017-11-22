library(ggplot2)
library(beadplot)
data <- data.frame(
  y = factor(c(1, 2, 2, 3, 3)),
  segment_id = c("A", "B", "C", "A", "D" ),
  dist = c(1, 2, 1, 2.5, 1))
segments <- data.frame(
  segment_id = LETTERS[1:4],
  min = c(0, 2, 0, 0),
  max = c(3, 4, 3, 2)
)

calc_offsets(segments, "group", "min", "max")
ggplot2::ggplot(data, ggplot2::aes(dist, y, segment_id = segment_id)) +
  stat_string(segments = segments) +
  stat_bead(segments = segments)
